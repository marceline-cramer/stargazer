use num::Num;

use crate::backend::*;

/// A backend which immediately evaluates to Rust values at runtime.
pub struct Interpreter;

impl Backend for Interpreter {}

impl Conditional for Interpreter {
    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T {
        if cond { if_true } else { if_false }
    }
}

impl FixedPoint for Interpreter {
    fn fixed_point<T>(&self, mut start: T, body: impl Fn(T) -> (Bool<Self>, T)) -> T {
        loop {
            match body(start) {
                (true, result) => return result,
                (false, result) => start = result,
            }
        }
    }
}

impl Scope<Interpreter> for bool {}

impl Function for Interpreter {
    type Function<'a, I: 'a, O: 'a> = &'a dyn Fn(I) -> O;
}

impl BuildFunction for Interpreter {
    fn build_function<'a, I, O>(
        &self,
        body: &'a dyn Fn(<Self as RustValue<I>>::Value) -> <Self as RustValue<O>>::Value,
    ) -> Self::Function<'a, I, O>
    where
        I: Scope<Self>,
        O: Scope<Self>,
        Self: RustValue<I> + RustValue<O>,
    {
        unsafe { core::mem::transmute(body) }
    }
}

impl RunFunction for Interpreter {
    fn run_function<I: Scope<Self>, O: Scope<Self>>(
        &self,
        func: &Self::Function<'_, I, O>,
        input: I,
    ) -> O {
        func(input)
    }
}

impl RustValue<bool> for Interpreter {
    type Value = bool;
}

impl<T: Primitive> RustValue<T> for Interpreter {
    type Value = T;
}

trait Primitive: Copy + Num + Scope<Interpreter> {}

macro_rules! impl_primitive {
    ($el:ident) => {};
    ($head:ident, $($tail:ident),+) => {
        impl Primitive for $head {}
        impl_primitive!($($tail),*);
    };
}

impl_primitive!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64);

#[cfg(test)]
mod tests {
    use super::*;

    use crate::tests::*;

    #[test]
    fn interpreter_core_tests() {
        let backend = Interpreter;
        core_tests(&backend);
    }
}
