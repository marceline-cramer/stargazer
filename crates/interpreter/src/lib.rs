use std::any::Any;

use stargazer_core::*;

/// A backend which immediately evaluates to Rust values at runtime.
pub struct Interpreter;

impl Backend for Interpreter {}

type InterpreterScope = Vec<Box<dyn Any>>;

impl<T: Primitive> Scope<InterpreterScope, T> for Interpreter {
    fn enter(&self, ctx: &mut InterpreterScope) -> T {
        *ctx.remove(0).downcast().unwrap()
    }

    fn leave(&self, ctx: &mut InterpreterScope, value: T) {
        ctx.push(Box::new(value));
    }
}

impl Conditional for Interpreter {
    type ConditionalScope<'a> = InterpreterScope;

    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T {
        if cond { if_true } else { if_false }
    }
}

impl FixedPoint for Interpreter {
    type FixedPointScope<'a> = InterpreterScope;

    fn fixed_point<'a, T>(&self, mut start: T, body: impl Fn(T) -> (Bool<'a, Self>, T)) -> T {
        loop {
            match body(start) {
                (true, result) => return result,
                (false, result) => start = result,
            }
        }
    }
}

impl Execute for Interpreter {
    type ExecuteScope<'a> = InterpreterScope;

    fn execute<'a, I: 'a, O: 'a>(&self, func: &ExecuteBody<'a, Self, I, O>) -> impl Fn(I) -> O + 'a
    where
        Self: RustValueScope<InterpreterScope, I> + RustValueScope<InterpreterScope, O>,
    {
        // TODO: implement this without transmutation
        let converted: &dyn Fn(I) -> O = unsafe { core::mem::transmute(func) };
        Box::new(converted)
    }
}

impl<T: Primitive> RustValue<T> for Interpreter {
    type Value<'a> = T;
}

trait Primitive: Copy + 'static {}

macro_rules! impl_primitive {
    ($el:ident) => {};
    ($head:ident, $($tail:ident),+) => {
        impl Primitive for $head {}
        impl_primitive!($($tail),*);
    };
}

impl_primitive!(
    bool, u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64
);

#[cfg(test)]
mod tests {
    use super::*;

    use stargazer_tests::*;

    #[test]
    fn interpreter_basic_tests() {
        let backend = Interpreter;
        basic_tests(&backend);
    }
}
