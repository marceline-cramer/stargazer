use core::fmt::Debug;

use num::{Bounded, Num};
use stargazer_core::*;

// TODO: Boolean logic tests
// TODO: rhs arithmetic tests

pub fn unsigned_arith_tests<
    B: Jit + RustNum<T> + 'static,
    T: Copy + Debug + From<u8> + Num + Bounded + Eq + 'static,
>(
    backend: &B,
) {
    let ty = std::any::type_name::<T>();

    test_assert_eq::<_, T, T>(
        format!("{ty} + 5"),
        backend,
        |_backend, lhs| lhs + T::from(5),
        &[
            (T::from(0), T::from(5)),
            (T::from(1), T::from(6)),
            (T::from(10), T::from(15)),
            (T::max_value() - T::from(5), T::max_value()),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} - {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs - rhs,
        &[
            ((T::from(10), T::from(5)), T::from(5)),
            ((T::from(100), T::from(50)), T::from(50)),
            ((T::from(255), T::from(1)), T::from(254)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} * {ty}"),
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
        format!("{ty} / {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs / rhs,
        &[
            ((T::from(10), T::from(2)), T::from(5)),
            ((T::from(100), T::from(10)), T::from(10)),
            ((T::from(255), T::from(2)), T::from(127)),
            // TODO: divide by zero?
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} % {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs % rhs,
        &[
            ((T::from(10), T::from(3)), T::from(1)),
            ((T::from(100), T::from(10)), T::from(0)),
            ((T::from(255), T::from(2)), T::from(1)),
            // TODO: modulo by zero?
        ],
    );
}

pub fn signed_arith_tests<
    B: Jit + RustNum<T> + 'static,
    T: Copy + Debug + From<i8> + Num + Bounded + Eq + 'static,
>(
    backend: &B,
) {
    let ty = std::any::type_name::<T>();

    test_assert_eq::<_, T, T>(
        format!("{ty} + 5"),
        backend,
        |_backend, lhs| lhs + T::from(5),
        &[
            (T::from(-5), T::from(0)),
            (T::from(0), T::from(5)),
            (T::from(10), T::from(15)),
            (T::from(-10), T::from(-5)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} - {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs - rhs,
        &[
            ((T::from(-5), T::from(5)), T::from(-10)),
            ((T::from(0), T::from(5)), T::from(-5)),
            ((T::from(10), T::from(10)), T::from(0)),
            ((T::from(-10), T::from(-5)), T::from(-5)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} * {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs * rhs,
        &[
            ((T::from(-5), T::from(5)), T::from(-25)),
            ((T::from(0), T::from(5)), T::from(0)),
            ((T::from(10), T::from(10)), T::from(100)),
            ((T::from(-10), T::from(-5)), T::from(50)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} / {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs / rhs,
        &[
            ((T::from(-5), T::from(5)), T::from(-1)),
            ((T::from(0), T::from(5)), T::from(0)),
            ((T::from(10), T::from(10)), T::from(1)),
            ((T::from(-10), T::from(-5)), T::from(2)),
        ],
    );

    test_assert_eq::<_, (T, T), T>(
        format!("{ty} % {ty}"),
        backend,
        |_backend, (lhs, rhs)| lhs % rhs,
        &[
            ((T::from(-5), T::from(5)), T::from(0)),
            ((T::from(0), T::from(5)), T::from(0)),
            ((T::from(10), T::from(10)), T::from(0)),
            ((T::from(-10), T::from(-5)), T::from(0)),
            // TODO: negative modulos?
            // TODO: modulo by zero?
        ],
    );
}

pub fn basic_tests<B: Jit + Basic + 'static>(backend: &B) {
    test_assert_eq::<B, u64, u64>(
        "fibonacci sequence",
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
    label: impl ToString,
    backend: &'a B,
    function: impl for<'b> JitBody<'b, B, I, O> + 'a,
    tests: &[(I, O)],
) where
    B: JitEnter<'a, I> + JitLeave<'a, O> + Jit,
    I: Clone + Debug + 'static,
    O: Debug + 'static,
    for<'b> &'b O: Eq,
{
    let label = label.to_string();
    let mut func = backend.jit(function);
    for (input, expected) in tests.iter() {
        let got = func(input.clone());
        if &got != expected {
            panic!("{label} failed: expected {expected:?}, got {got:?}, input {input:?}");
        }
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
