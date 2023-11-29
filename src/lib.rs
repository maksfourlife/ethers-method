use ethers_contract_derive::{EthAbiCodec, EthAbiType};
use ethers_core::{
    abi::{AbiDecode, AbiEncode, AbiError, AbiType, ParamType},
    types::{transaction::eip2718::TypedTransaction, Address, BlockId, Bytes, NameOrAddress, U256},
};
use ethers_providers::Middleware;
use once_cell::sync::Lazy;
use std::{
    fmt::{self, Debug},
    marker::PhantomData,
    ops::Deref,
};

pub use ethers_core;
pub use once_cell;

#[derive(Debug, thiserror::Error)]
pub enum Error<M: Middleware> {
    #[error(transparent)]
    Middleware(M::Error),
    #[error(transparent)]
    Abi(#[from] AbiError),
}

#[derive(Clone, Copy)]
pub struct Method<A, R> {
    args: A,
    vtable: &'static MethodVTable,
    _p: PhantomData<R>,
}

pub struct MethodVTable {
    pub name: &'static str,
    pub selector: Lazy<[u8; 4]>,
    pub debug_args: unsafe fn(*const (), &mut fmt::Formatter<'_>) -> fmt::Result,
}

impl<A: fmt::Debug, R> fmt::Debug for Method<A, R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Args<'a, A> {
            args: &'a A,
            debug: unsafe fn(*const (), &mut fmt::Formatter<'_>) -> fmt::Result,
        }
        impl<'a, A> fmt::Debug for Args<'a, A> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                unsafe { (self.debug)(self.args as *const A as *const (), f) }
            }
        }
        f.debug_struct("Method")
            .field("selector", &hex::encode(self.vtable.selector.deref()))
            .field(
                "args",
                &Args {
                    args: &self.args,
                    debug: self.vtable.debug_args,
                },
            )
            .field("_name", &self.vtable.name)
            .field("_p", &self._p)
            .finish()
    }
}

impl<A, R> Method<A, R> {
    pub fn new(vtable: &'static MethodVTable, args: A) -> Self {
        Self {
            args,
            vtable,
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

    pub fn encode_args(self) -> Vec<u8>
    where
        A: AbiEncode,
    {
        [self.vtable.selector.to_vec(), self.args.encode()].concat()
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
        let data = provider
            .call(&self.tx, block)
            .await
            .map_err(Error::Middleware)?;
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

    pub fn into_raw(self) -> RawCall
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
            use $crate::{
                ethers_core::abi::{short_signature, AbiType},
                once_cell::sync::Lazy,
                Method, MethodVTable,
            };
            unsafe fn debug_args(args: *const (), f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let ( $($arg_name),+ ,) = &*(args as *const ($($arg_type),+,));
                f.debug_struct("Args")
                    $(.field(stringify!($arg_name), $arg_name))+
                    .finish()
            }
            static VTABLE: MethodVTable = MethodVTable {
                selector: Lazy::new(|| {
                    short_signature($name, &[$(<$arg_type as AbiType>::param_type()),+])
                }),
                debug_args,
                name: $name
            };
            Method::new(&VTABLE, ($($arg_name),+,))
        }
    };

    ($alias:ident, $($vis: vis,)? $name:expr, $return_type:ty) => {
        #[allow(unused)]
        $($vis)? fn $alias() -> $crate::Method<$crate::Zst, $return_type> {
            use $crate::{
                ethers_core::abi::{short_signature, AbiType},
                once_cell::sync::Lazy,
                Method, MethodVTable, Zst
            };
            fn debug_args(_args: *const (), f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("Args").finish()
            }
            static VTABLE: MethodVTable = MethodVTable {
                selector: Lazy::new(|| short_signature($name, &[])),
                debug_args,
                name: $name
            };
            Method::new(&VTABLE, Zst)
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
    calls: C,
    tx: TypedTransaction,
}

impl<C: Calls> Multicall<C> {
    pub fn new(calls: C) -> Self {
        Self {
            calls,
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
        let calls = self.calls.encode();
        let data = multicall(calls).encode_args();
        self.tx.set_data(data.into());
        provider.estimate_gas(&self.tx, block).await
    }

    pub async fn call<P: Middleware>(
        mut self,
        provider: &P,
        block: Option<BlockId>,
    ) -> Result<(U256, C::Results), Error<P>> {
        let calls = self.calls.encode();
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
        impl_method!(nonce, pub, "nonce", Zst);

        let method = multicall(vec![]);
        dbg!(&method);

        let method = nonce();
        dbg!(&method);
    }
}
