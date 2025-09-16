use core::fmt::Debug;

use num::{Bounded, Num};
use stargazer_core::*;

pub fn unsigned_arith_tests<T: From<u8> + Num + Bounded>(backend: &(impl Jit + RustNum<T>)) {}

pub fn signed_arith_tests<T: From<u8> + Num + Bounded>(backend: &(impl Jit + RustNum<T>)) {}

fn add<'a, T: Num, B: RustNum<T>>(
    _backend: &B,
    lhs: <B as RustValue<T>>::Value<'a>,
    rhs: <B as RustValue<T>>::Value<'a>,
) -> <B as RustValue<T>>::Value<'a> {
    lhs + rhs
}

pub fn basic_tests<B: Jit + Basic>(backend: &B) {
    test_assert_eq::<B, u64, u64>(
        backend,
        fib,
        &[
            (0, 0),
            (1, 1),
            (2, 1),
            (3, 2),
            (4, 3),
            (5, 5),
            (6, 8),
            (7, 13),
            (8, 21),
            (9, 34),
        ],
    );
}

pub fn test_assert_eq<'a, B, I, O>(
    backend: &'a B,
    function: impl Fn(&B, <B as RustValue<I>>::Value<'a>) -> <B as RustValue<O>>::Value<'a> + 'a,
    tests: &[(I, O)],
) where
    B: JitEnter<I> + JitLeave<O> + Jit,
    I: Clone + Debug + 'static,
    O: Debug + 'static,
    for<'b> &'b O: Eq,
{
    let wrapped = move |input| function(backend, input);
    let func = backend.jit(wrapped);
    for (input, expected) in tests.iter() {
        let got = func(input.clone());
        assert_eq!(expected, &got, "input {input:?}");
    }
}

fn fib<'a, B: FixedPoint + Conditional + RustNum<u64>>(backend: &B, num: U64<'a, B>) -> U64<'a, B> {
    let start = (U64::<B>::from(0), U64::<B>::from(0), U64::<B>::from(1));

    let (_, last, _) = backend.fixed_point(start, |(mut idx, a, b)| {
        idx += 1;
        let c = a + b;
        let stop = num.lt(&idx);
        let (at, next) = backend.conditional(stop, (a, b), (b, c));
        (stop, (idx, at, next))
    });

    last
}
