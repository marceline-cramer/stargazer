use std::{
    cell::RefCell,
    marker::PhantomData,
    ops::{Add, Div, Mul, Not, Rem, Sub},
};

use stargazer_core::*;

#[derive(Clone, Debug, Default)]
pub struct WasmBackend {}

impl Backend for WasmBackend {}

impl<T: AsWasm> RustValue<T> for WasmBackend {
    type Value<'a> = MaybeConst<T, WasmInteger<'a, T>>;
}

pub enum WasmPrimitiveKind {
    I32,
    I64,
    F32,
    F64,
}

pub struct WasmInteger<'a, T> {
    id: usize,
    ctx: &'a Context<'a>,
    _phantom: PhantomData<T>,
}

struct ContextInner {}

pub struct Context<'a> {
    backend: &'a WasmBackend,
    inner: RefCell<ContextInner>,
}

impl<'a, T> Copy for WasmInteger<'a, T> {}

impl<T: AsWasm> Scope<T> for WasmBackend {
    fn visit<C>(&self, ctx: &mut C, value: &T) {
        todo!()
    }
}

impl<'a, T> Clone for WasmInteger<'a, T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            ctx: self.ctx,
            _phantom: PhantomData,
        }
    }
}

impl<'a> Not for WasmInteger<'a, bool> {
    type Output = Self;

    fn not(self) -> Self::Output {
        todo!()
    }
}

pub trait AsWasm: Copy {
    type Primitive;
    const KIND: WasmPrimitiveKind;

    fn push(&self);
}

impl AsWasm for bool {
    type Primitive = bool;
    const KIND: WasmPrimitiveKind = WasmPrimitiveKind::I32;

    fn push(&self) {
        todo!()
    }
}

impl<'a, T: AsWasm> AsWasm for WasmInteger<'a, T> {
    type Primitive = T;
    const KIND: WasmPrimitiveKind = T::KIND;

    fn push(&self) {
        todo!()
    }
}

macro_rules! impl_rhs_op {
    ($prim:ty, $op:ident, $method:ident) => {
        impl<'a> $op<WasmInteger<'a, $prim>> for $prim {
            type Output = WasmInteger<'a, $prim>;

            fn $method(self, other: WasmInteger<'a, $prim>) -> WasmInteger<'a, $prim> {
                todo!()
            }
        }
    };
}

macro_rules! impl_as_wasm {
    ($kind:expr,) => {};
    ($kind:expr, $head:ty $(, $tail:ty)*) => {
        impl AsWasm for $head {
            type Primitive = $head;
            const KIND: WasmPrimitiveKind = $kind;

            fn push(&self) {
                todo!()
            }
        }

        impl_rhs_op!($head, Sub, sub);
        impl_rhs_op!($head, Div, div);
        impl_rhs_op!($head, Rem, rem);

        impl_as_wasm!($kind, $($tail),*);
    };
}

impl_as_wasm!(WasmPrimitiveKind::I32, u8, u16, u32, i8, i16, i32);
impl_as_wasm!(WasmPrimitiveKind::I64, u64, i64);
impl_as_wasm!(WasmPrimitiveKind::F32, f32);
impl_as_wasm!(WasmPrimitiveKind::F64, f64);

impl<'a, T: AsWasm> Compare<'a, T, WasmBackend> for WasmInteger<'a, T> {
    fn eq(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn le(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn lt(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }
}

impl<'a, T: AsWasm> Compare<'a, Self, WasmBackend> for WasmInteger<'a, T> {
    fn eq(&self, rhs: &Self) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn le(&self, rhs: &Self) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn lt(&self, rhs: &Self) -> Bool<'a, WasmBackend> {
        todo!()
    }
}

impl<'a, T: AsWasm> Add<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Mul<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Sub<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Div<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn div(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Rem<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn rem(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl Conditional for WasmBackend {
    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T
    where
        Self: Scope<T>,
    {
        let mut bleh = 0;
        self.visit(&mut bleh, &if_true);
        todo!()
    }
}

impl FixedPoint for WasmBackend {
    fn fixed_point<'a, T>(&self, start: T, body: impl Fn(T) -> (Bool<'a, Self>, T)) -> T
    where
        Self: Scope<T>,
    {
        todo!()
    }
}

impl Execute for WasmBackend {
    fn execute<I, O>(&self, func: &ExecuteBody<'_, Self, I, O>, input: I) -> O
    where
        Self: RustValue<I> + RustValue<O>,
    {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use stargazer_tests::unsigned_arith_tests;

    #[test]
    fn wasm_unsigned_arith() {
        let backend = WasmBackend::default();
        unsigned_arith_tests::<u32>(&backend);
    }
}
