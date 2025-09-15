use num::Num;
use stargazer_core::*;

/// A backend which immediately evaluates to Rust values at runtime.
pub struct Interpreter;

impl Backend for Interpreter {}

impl Conditional for Interpreter {
    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T {
        if cond { if_true } else { if_false }
    }
}

impl FixedPoint for Interpreter {
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
    fn execute<I, O>(&self, func: &ExecuteBody<'_, Self, I, O>, input: I) -> O
    where
        Self: RustValue<I> + RustValue<O>,
    {
        let converted: &dyn Fn(I) -> O = unsafe { core::mem::transmute(func) };
        converted(input)
    }
}

impl Scope<bool> for Interpreter {
    fn visit<C>(&self, _: &mut C, _: &bool) {}
}

impl<T: Primitive> Scope<T> for Interpreter {
    fn visit<C>(&self, _: &mut C, _: &T) {}
}

impl RustValue<bool> for Interpreter {
    type Value<'a> = bool;
}

impl<T: Primitive> RustValue<T> for Interpreter {
    type Value<'a> = T;
}

trait Primitive: Copy + Num {}

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

    use stargazer_tests::*;

    #[test]
    fn interpreter_basic_tests() {
        let backend = Interpreter;
        basic_tests(&backend);
    }
}
