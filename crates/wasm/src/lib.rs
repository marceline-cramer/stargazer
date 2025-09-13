use std::{
    marker::PhantomData,
    ops::{Add, Not},
};

use stargazer_core::*;

#[derive(Clone, Debug, Default)]
pub struct WasmBackend {}

impl Backend for WasmBackend {}

impl<T: Primitive> RustValue<T> for WasmBackend {
    type Value = MaybeConst<T, WasmInteger<T>>;
}

impl RustValue<bool> for WasmBackend {
    type Value = MaybeConst<bool, WasmInteger<i32>>;
}

pub struct WasmInteger<T> {
    id: usize,
    _phantom: PhantomData<T>,
}

impl<T: Primitive> Copy for WasmInteger<T> {}

impl<T: Primitive> Scope<WasmBackend> for WasmInteger<T> {}

impl<T: Primitive> Clone for WasmInteger<T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            _phantom: PhantomData,
        }
    }
}

impl Not for WasmInteger<i32> {
    type Output = Self;

    fn not(self) -> Self::Output {
        todo!()
    }
}

macro_rules! impl_marker {
    ($marker:ident,) => {};
    ($marker:ident, $head:ty $(, $tail:ty)*) => {
        impl $marker for $head {}
        impl_marker!($marker, $($tail),*);
    };
}

trait Primitive: Copy {}

impl_marker!(
    Primitive, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64
);

trait RepI32: Primitive {}

impl_marker!(RepI32, u8, u16, u32, i8, i16, i32);

trait RepI64: Primitive {}

impl_marker!(RepI64, u64, i64);

trait RepF32: Primitive {}

impl_marker!(RepF32, f32);

trait RepF64: Primitive {}

impl_marker!(RepF64, f64);

impl<T: RepI32> Add<T> for WasmInteger<T> {
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<T: RepI32> Add for WasmInteger<T> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

impl Conditional for WasmBackend {
    fn conditional<T: Scope<Self>>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T {
        todo!()
    }
}

impl FixedPoint for WasmBackend {
    fn fixed_point<T: Scope<Self>>(&self, start: T, body: impl Fn(T) -> (Bool<Self>, T)) -> T {
        todo!()
    }
}

impl Execute for WasmBackend {
    fn execute<I, O>(
        &self,
        func: &dyn Fn(&Self, <Self as RustValue<I>>::Value) -> <Self as RustValue<O>>::Value,
        input: I,
    ) -> O
    where
        Self: RustValue<I> + RustValue<O>,
    {
        todo!()
    }
}
