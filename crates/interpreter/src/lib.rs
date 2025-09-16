use std::any::Any;

use stargazer_core::*;

/// A backend which immediately evaluates to Rust values at runtime.
pub struct Interpreter;

impl Backend for Interpreter {}

type InterpreterScope = Vec<Box<dyn Any>>;

impl<T: Primitive> Enter<InterpreterScope, T> for Interpreter {
    fn enter(&self, ctx: &mut InterpreterScope) -> T {
        *ctx.pop().unwrap().downcast().unwrap()
    }
}

impl<T: Primitive> Leave<InterpreterScope, T> for Interpreter {
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

impl JitScopes for Interpreter {
    type InputScope<'a> = InterpreterScope;
    type FuncScope<'a> = InterpreterScope;
    type OutputScope<'a> = InterpreterScope;
}

impl Jit for Interpreter {
    fn jit<'a, I, O>(&'a self, body: impl JitBody<'a, Self, I, O>) -> impl Fn(I) -> O + 'a
    where
        Self: JitEnter<I> + JitLeave<O>,
    {
        move |inputs| {
            let mut scope = InterpreterScope::new();
            self.leave(&mut scope, inputs);
            scope.reverse();
            let inputs = self.enter(&mut scope);

            let outputs = body(inputs);

            let mut scope = InterpreterScope::new();
            self.leave(&mut scope, outputs);
            scope.reverse();
            self.enter(&mut scope)
        }
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
