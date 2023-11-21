use std::{
    fmt::{self, Debug},
    future::Future,
    marker::PhantomData,
    ops::Deref,
    pin::Pin,
    task::{ready, Context, Poll},
};

use ethers_contract_derive::{EthAbiCodec, EthAbiType};
use ethers_core::{
    abi::{AbiDecode, AbiEncode, AbiError, AbiType, ParamType},
    types::{transaction::eip2718::TypedTransaction, Address, BlockId, Bytes, NameOrAddress, U256},
};
use ethers_providers::{spoof::State, Middleware, MiddlewareError};
use once_cell::sync::Lazy;

pub use ethers_core;
pub use once_cell;
use pin_project::pin_project;

#[derive(Debug, thiserror::Error)]
pub enum Error<M: Middleware> {
    #[error(transparent)]
    Middleware(M::Error),
    #[error(transparent)]
    Abi(#[from] AbiError),
}

#[derive(Clone, Copy)]
pub struct Method<A, R> {
    selector: &'static Lazy<[u8; 4]>,
    args: Option<A>,
    _debug_args: fn(&A, &mut fmt::Formatter<'_>) -> fmt::Result,
    // _arg_names: &'static [&'static str],
    _name: &'static str,
    _p: PhantomData<R>,
}

impl<A: fmt::Debug, R> fmt::Debug for Method<A, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Args<'a, A> {
            args: &'a A,
            debug: fn(&'a A, &mut fmt::Formatter<'_>) -> fmt::Result,
        }
        impl<'a, A> fmt::Debug for Args<'a, A> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                (self.debug)(self.args, f)
            }
        }
        f.debug_struct("Method")
            .field("selector", &hex::encode(self.selector.deref()))
            .field(
                "args",
                &self.args.as_ref().map(|args| Args {
                    args,
                    debug: self._debug_args,
                }),
            )
            .field("_name", &self._name)
            .field("_p", &self._p)
            .finish()
    }
}

impl<A, R> Method<A, R> {
    pub fn new(
        _name: &'static str,
        selector: &'static Lazy<[u8; 4]>,
        // _arg_names: &'static [&'static str],
        _debug_args: fn(&A, &mut fmt::Formatter<'_>) -> fmt::Result,
        args: A,
    ) -> Self {
        Self {
            selector,
            args: Some(args),
            // _arg_names,
            _debug_args,
            _name,
            _p: PhantomData,
        }
    }

    pub fn tx(self) -> MethodTx<A, R> {
        MethodTx {
            method: self,
            tx: Default::default(),
        }
    }

    pub fn call(self, to: Address, op: Operation) -> MethodCall<A, R> {
        MethodCall {
            method: self,
            to,
            op,
            required: true,
            value: U256::zero(),
        }
    }

    pub fn encode_args(&mut self) -> Vec<u8>
    where
        A: AbiEncode,
    {
        [self.selector.to_vec(), self.args.take().unwrap().encode()].concat()
    }
}

#[derive(Debug, Clone)]
pub struct MethodTx<A, R> {
    method: Method<A, R>,
    tx: TypedTransaction,
}

impl<A, R> MethodTx<A, R> {
    pub fn to(mut self, to: impl Into<NameOrAddress>) -> Self {
        self.tx.set_to(to);
        self
    }

    pub fn from(mut self, from: Address) -> Self {
        self.tx.set_from(from);
        self
    }

    pub async fn call<M: Middleware>(
        mut self,
        provider: &M,
        block: Option<BlockId>,
    ) -> Result<R, Error<M>>
    where
        A: AbiEncode + fmt::Debug,
        R: AbiDecode + fmt::Debug,
    {
        let data = self.method.encode_args();
        self.tx.set_data(data.into());
        let data = provider.call(&self.tx, block).await.map_err(|e| {
            dbg!(&e, &self.method);
            Error::Middleware(e)
        })?;
        Ok(AbiDecode::decode(data)?)
    }

    pub async fn estimate_gas<M: Middleware>(
        mut self,
        provider: &M,
        block: Option<BlockId>,
    ) -> Result<U256, Error<M>>
    where
        A: AbiEncode,
    {
        let data = self.method.encode_args();
        self.tx.set_data(data.into());
        provider
            .estimate_gas(&self.tx, block)
            .await
            .map_err(Error::Middleware)
    }

    #[allow(clippy::type_complexity)]
    pub fn call_raw<'a, M: Middleware>(
        &'a mut self,
        provider: &'a M,
    ) -> CallBuilder<'a, M, fn(Vec<u8>) -> Result<R, AbiError>>
    where
        A: AbiEncode,
        R: AbiDecode,
    {
        let data = self.method.encode_args();
        self.tx.set_data(data.into());
        CallBuilder::new(provider.provider().call_raw(&self.tx), |data| {
            R::decode(data)
        })
    }
}

#[pin_project]
pub struct CallBuilder<'a, M: Middleware, F> {
    #[pin]
    builder: ethers_providers::CallBuilder<'a, M::Provider>,
    f: F,
}

impl<'a, M: Middleware, F> CallBuilder<'a, M, F> {
    pub fn new(builder: ethers_providers::CallBuilder<'a, M::Provider>, f: F) -> Self {
        Self { builder, f }
    }
}

impl<'a, M: Middleware, R> ethers_providers::RawCall<'a> for CallBuilder<'a, M, R> {
    fn block(self, id: BlockId) -> Self {
        let builder = self.builder.block(id);
        Self::new(builder, self.f)
    }

    fn state(self, state: &'a State) -> Self {
        let builder = self.builder.state(state);
        Self::new(builder, self.f)
    }
}

impl<'a, M: Middleware, F: Fn(Vec<u8>) -> Result<R, AbiError>, R> Future for CallBuilder<'a, M, F> {
    type Output = Result<R, Error<M>>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        let data = ready!(this.builder.poll(cx));
        Poll::Ready(
            data.map_err(<M::Error as MiddlewareError>::from_provider_err)
                .map_err(Error::<M>::Middleware)
                .and_then(|data| (this.f)(data.to_vec()).map_err(Error::Abi)),
        )
    }
}

#[derive(Debug, Clone, Copy, EthAbiCodec, EthAbiType)]
pub enum Operation {
    Call,
    DelegateCall,
}

impl AbiType for Operation {
    fn param_type() -> ParamType {
        <u8 as AbiType>::param_type()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MethodCall<A, R> {
    pub method: Method<A, R>,
    pub required: bool,
    pub op: Operation,
    pub to: Address,
    pub value: U256,
}

impl<A, R> MethodCall<A, R> {
    pub fn value(mut self, value: U256) -> Self {
        self.value = value;
        self
    }

    pub fn into_raw(mut self) -> RawCall
    where
        A: AbiEncode,
    {
        RawCall {
            required: self.required,
            op: self.op,
            to: self.to,
            value: self.value,
            data: self.method.encode_args().into(),
        }
    }
}

#[derive(Debug, Clone, EthAbiCodec, EthAbiType)]
pub struct RawCall {
    pub op: Operation,
    pub required: bool,
    pub to: Address,
    pub value: U256,
    pub data: Bytes,
}

#[derive(Debug, Clone, EthAbiCodec, EthAbiType)]
pub struct RawResult {
    pub success: bool,
    pub data: Bytes,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Zst;

impl AbiEncode for Zst {
    fn encode(self) -> Vec<u8> {
        vec![]
    }
}

impl AbiDecode for Zst {
    fn decode(_: impl AsRef<[u8]>) -> Result<Self, AbiError> {
        Ok(Zst)
    }
}

#[macro_export]
macro_rules! impl_method {
    ($alias:ident, $($vis: vis,)? $name:expr, $return_type:ty, $($arg_name:ident: $arg_type:ty),+) => {
        #[allow(unused, clippy::too_many_arguments)]
        $($vis)? fn $alias($($arg_name: $arg_type),+) -> $crate::Method<($($arg_type),+,), $return_type> {
            use $crate::once_cell::sync::Lazy;
            use $crate::ethers_core::abi::{short_signature, AbiType};
            static SELECTOR: Lazy<[u8; 4]> = Lazy::new(|| {
                short_signature($name, &[$(<$arg_type as AbiType>::param_type()),+])
            });
            // const ARG_NAMES: &[&str] = &[ $(stringify!($arg_name),)+ ];
            // $crate::Method::new($name, &SELECTOR, ARG_NAMES, ($($arg_name),+,))
            fn debug_args(args: &( $($arg_type),+ ,), f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let ( $($arg_name),+ ,) = args;
                f.debug_struct("Args")
                    $(.field(stringify!($arg_name), $arg_name))+
                    .finish()
            }
            $crate::Method::new($name, &SELECTOR, debug_args, ($($arg_name),+,))
        }
    };

    ($alias:ident, $($vis: vis,)? $name:expr, $return_type:ty) => {
        #[allow(unused)]
        $($vis)? fn $alias() -> $crate::Method<$crate::Zst, $return_type> {
            use $crate::once_cell::sync::Lazy;
            use $crate::ethers_core::abi::{short_signature};
            static SELECTOR: Lazy<[u8; 4]> = Lazy::new(|| short_signature($name, &[]));
            fn debug_args(args: &$crate::Zst, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Args").finish()
            }
            $crate::Method::new($name, &SELECTOR, debug_args, $crate::Zst)
        }
    };
}

impl_method!(
    multicall,
    pub,
    "multicall",
    (U256, Vec<RawResult>),
    calls: Vec<RawCall>
);

#[derive(Clone, Debug, Default)]
pub struct Multicall<C> {
    calls: Option<C>,
    tx: TypedTransaction,
}

impl<C: Calls> Multicall<C> {
    pub fn new(calls: C) -> Self {
        Self {
            calls: Some(calls),
            tx: Default::default(),
        }
    }

    pub fn to(mut self, to: impl Into<NameOrAddress>) -> Self {
        self.tx.set_to(to);
        self
    }

    pub fn from(mut self, from: Address) -> Self {
        self.tx.set_from(from);
        self
    }

    pub async fn estimate_gas<P: Middleware>(
        mut self,
        provider: &P,
        block: Option<BlockId>,
    ) -> Result<U256, P::Error> {
        let calls = self.calls.take().unwrap().encode();
        let data = multicall(calls).encode_args();
        self.tx.set_data(data.into());
        provider.estimate_gas(&self.tx, block).await
    }

    pub async fn call<P: Middleware>(
        mut self,
        provider: &P,
        block: Option<BlockId>,
    ) -> Result<(U256, C::Results), Error<P>> {
        let calls = self.calls.take().unwrap().encode();
        let value = calls
            .iter()
            .fold(U256::zero(), |value, call| value + call.value);
        let data = multicall(calls).encode_args();
        self.tx.set_data(data.into());
        self.tx.set_value(value);
        let data = provider
            .call(&self.tx, block)
            .await
            .map_err(Error::Middleware)?;
        let (gas_used, results): (U256, Vec<RawResult>) = AbiDecode::decode(data)?;
        Ok((gas_used, C::decode(results)?))
    }

    #[allow(clippy::type_complexity)]
    pub fn call_raw<'a, M: Middleware>(
        &'a mut self,
        provider: &'a M,
    ) -> CallBuilder<'a, M, fn(Vec<u8>) -> Result<C::Results, AbiError>> {
        let calls = self.calls.take().unwrap().encode();
        let value = calls
            .iter()
            .fold(U256::zero(), |value, call| value + call.value);
        let data = multicall(calls).encode_args();
        self.tx.set_data(data.into());
        self.tx.set_value(value);
        CallBuilder::new(provider.provider().call_raw(&self.tx), |data| {
            C::decode(AbiDecode::decode(data)?)
        })
    }
}

pub trait Calls: fmt::Debug {
    type Results;
    fn encode(self) -> Vec<RawCall>;
    fn decode(results: Vec<RawResult>) -> Result<Self::Results, AbiError>
    where
        Self: Sized;
}

impl Calls for Vec<RawCall> {
    type Results = Vec<RawResult>;

    fn encode(self) -> Vec<RawCall> {
        self
    }

    fn decode(results: Vec<RawResult>) -> Result<Self::Results, AbiError> {
        Ok(results)
    }
}

macro_rules! impl_tuples {
    ($no0:tt : ($a0:ident, $r0:ident), $($no:tt : ($a:ident, $r:ident) ),+) => {
        impl<
            $a0: AbiEncode + fmt::Debug,
            $r0: AbiDecode + fmt::Debug,
            $( $a: AbiEncode + fmt::Debug, $r: AbiDecode + fmt::Debug ),+
        > Calls for ( MethodCall<$a0, $r0>, $( MethodCall<$a, $r>, )+ )
        {
            type Results = ( $r0, $( $r, )+ );

            fn encode(self) -> Vec<RawCall> {
                let mut raw_calls = vec![self.$no0.into_raw(), $(self.$no.into_raw()),+];
                raw_calls.reverse();
                raw_calls
            }

            fn decode(mut results: Vec<RawResult>) -> Result<Self::Results, AbiError> {
                Ok((
                    $r0::decode(results.pop().unwrap().data)?,
                    $($r::decode(results.pop().unwrap().data)?),+
                ))
            }
        }
        impl_tuples!($($no : ($a, $r) ),+);
    };

    ($no0:tt : ($a0:ident, $r0:ident)) => {
        impl<
            $a0: AbiEncode + fmt::Debug,
            $r0: AbiDecode + fmt::Debug,
        > Calls for (MethodCall<$a0, $r0>,)
        {
            type Results = ($r0,);

            fn encode(self) -> Vec<RawCall> {
                vec![self.$no0.into_raw()]
            }

            fn decode(mut results: Vec<RawResult>) -> Result<Self::Results, AbiError> {
                Ok((
                    $r0::decode(results.pop().unwrap().data)?,
                ))
            }
        }
    };
}

impl_tuples!(
    // 16: (A16, R16),
    // 15: (A15, R15),
    // 14: (A14, R14),
    // 13: (A13, R13),
    // 12: (A12, R12),
    11: (A11, R11),
    10: (A10, R10),
    9: (A9, R9),
    8: (A8, R8),
    7: (A7, R7),
    6: (A6, R6),
    5: (A5, R5),
    4: (A4, R4),
    3: (A3, R3),
    2: (A2, R2),
    1: (A1, R1),
    0: (A0, R0)
);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_args_debug() {
        let method = multicall(vec![]);
        dbg!(&method);
    }
}
