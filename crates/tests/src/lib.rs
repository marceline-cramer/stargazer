use core::fmt::Debug;

use num::{Bounded, Num};
use stargazer_core::*;

pub fn unsigned_arith_tests<
    B: Jit + RustNum<T> + 'static,
    T: Copy + Debug + From<u8> + Num + Bounded + Eq + 'static,
>(
    backend: &B,
) {
    test_assert_eq::<_, T, T>(
        backend,
        |_backend, lhs| lhs + <B as RustValue<T>>::Value::from(5.into()),
        &[
            (T::from(0), T::from(5)),
            (T::from(1), T::from(6)),
            (T::from(10), T::from(15)),
            (T::from(255), T::from(255) + T::from(5)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        backend,
        |_backend, (lhs, rhs)| add(backend, lhs, rhs),
        &[
            ((T::from(0), T::from(0)), T::from(0)),
            ((T::from(1), T::from(2)), T::from(3)),
            ((T::from(10), T::from(20)), T::from(30)),
            ((T::from(100), T::from(155)), T::from(255)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        backend,
        |_backend, (lhs, rhs)| lhs * rhs,
        &[
            ((T::from(0), T::from(5)), T::from(0)),
            ((T::from(1), T::from(1)), T::from(1)),
            ((T::from(2), T::from(3)), T::from(6)),
            ((T::from(10), T::from(10)), T::from(100)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        backend,
        |_backend, (lhs, rhs)| lhs - rhs,
        &[
            ((T::from(10), T::from(5)), T::from(5)),
            ((T::from(100), T::from(50)), T::from(50)),
            ((T::from(255), T::from(1)), T::from(254)),
        ],
    );
}

pub fn signed_arith_tests<T: From<u8> + Num + Bounded>(backend: &(impl Jit + RustNum<T>)) {}

fn add<'a, T: Num, B: RustNum<T>>(
    _backend: &B,
    lhs: <B as RustValue<T>>::Value<'a>,
    rhs: <B as RustValue<T>>::Value<'a>,
) -> <B as RustValue<T>>::Value<'a> {
    lhs + rhs
}

pub fn basic_tests<B: Jit + Basic + 'static>(backend: &B) {
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
    function: impl for<'b> JitBody<'b, B, I, O> + 'a,
    tests: &[(I, O)],
) where
    B: JitEnter<'a, I> + JitLeave<'a, O> + Jit,
    I: Clone + Debug + 'static,
    O: Debug + 'static,
    for<'b> &'b O: Eq,
{
    let mut func = backend.jit(function);
    for (input, expected) in tests.iter() {
        let got = func(input.clone());
        assert_eq!(expected, &got, "input {input:?}");
    }
}

fn fib<'a, B: RustNum<u64>>(backend: &'a B, num: U64<'a, B>) -> U64<'a, B> {
    let start = (U64::<B>::from(0), U64::<B>::from(0), U64::<B>::from(1));

    let (_, last, _) = backend.fixed_point::<(_, _, _)>(start, |(mut idx, a, b)| {
        idx += 1;
        let c = a + b;
        let stop = backend.lt(num, idx);
        let (at, next) = backend.conditional::<(_, _)>(stop, (a, b), (b, c));
        (stop, (idx, at, next))
    });

    last
}
