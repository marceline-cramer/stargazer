use core::fmt::Debug;

use crate::backend::*;

pub fn core_tests<B: BuildFunction + Conditional + RunFunction + HasIntegers + FixedPoint>(
    backend: &B,
) {
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

pub fn test_assert_eq<B, I, O>(
    backend: &B,
    function: impl Fn(&B, <B as RustValue<I>>::Value) -> <B as RustValue<O>>::Value,
    tests: &[(I, O)],
) where
    B: BuildFunction + RunFunction + RustValue<I> + RustValue<O>,
    I: Clone + Debug + Scope<B>,
    O: Debug + Scope<B>,
    for<'a> &'a O: Eq,
{
    let wrapped = |input| function(backend, input);
    let built = backend.build_function(&wrapped);
    for (input, expected) in tests.iter() {
        let got = backend.run_function(&built, input.clone());
        assert_eq!(expected, &got, "input {input:?}");
    }
}

fn fib<B: FixedPoint + Conditional + HasU64>(backend: &B, num: U64<B>) -> U64<B> {
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
