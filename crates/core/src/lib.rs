#![no_std]

use core::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Not, Rem, RemAssign, Sub, SubAssign,
};

use num::traits::{NumAssignOps, NumOps};

/// The supertrait of all backend functionality.
pub trait Backend {}

/// A backend trait associating a Rust type with some backend type.
///
/// Used for primitive numeric types (i\*, u\*, f\*).
pub trait RustValue<T>: Backend {
    type Value<'a>: Copy + Scope<Self>;
}

/// The backend supports initializing values from Rust constants.
pub trait RustConst<T>: for<'a> RustValue<T, Value<'a>: From<T>> {}

/// Blanket implementation for RustConst.
impl<T, B: RustValue<T>> RustConst<T> for B where for<'a> B::Value<'a>: From<T> {}

/// Annotates that the Boolean type is fully supported by a backend.
pub trait HasBool: for<'a> RustConst<bool, Value<'a>: SelfNot> {}

/// Necessary to break this out to avoid associated type bound on generic lifetime in [HasBool].
pub trait SelfNot: Not<Output = Self> {}

impl<T: Not<Output = T>> SelfNot for T {}

/// Blanket implementation for HasBool.
impl<B: RustConst<bool>> HasBool for B where for<'a> B::Value<'a>: SelfNot {}

/// Supports comparing two Rust values and returning a Boolean.
pub trait RustCmp<T>:
    for<'a> RustValue<
        T,
        Value<'a>: Compare<'a, <Self as RustValue<T>>::Value<'a>, Self> + Compare<'a, T, Self>,
    > + HasBool
{
}

/// Blanket implementation for comparables.
impl<T, B> RustCmp<T> for B
where
    B: RustValue<T> + HasBool,
    for<'a> <B as RustValue<T>>::Value<'a>:
        Compare<'a, <B as RustValue<T>>::Value<'a>, B> + Compare<'a, T, B>,
{
}

/// Annotates a backend's Rust value type as supporting basic numeric operations.
pub trait RustNum<T: NumOps>:
    RustCmp<T>
    + RustConst<T>
    + for<'a> RustValue<T, Value<'a>: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>>
{
}

/// Blanket implementation for RustNum.
impl<T, B> RustNum<T> for B
where
    B: RustCmp<T> + RustConst<T>,
    for<'a> <B as RustValue<T>>::Value<'a>: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>,
    T: NumOps + for<'a> NumOps<<B as RustValue<T>>::Value<'a>>,
{
}

/// Type alias for a backend's Boolean value.
pub type Bool<'a, T> = <T as RustValue<bool>>::Value<'a>;

/// Comparison operations for backend values.
pub trait Compare<'a, Rhs, B: HasBool + ?Sized> {
    fn eq(&self, rhs: &Rhs) -> Bool<'a, B>;
    fn le(&self, rhs: &Rhs) -> Bool<'a, B>;
    fn lt(&self, rhs: &Rhs) -> Bool<'a, B>;
}

/// Blanket implementation for `Ord` types.
impl<'a, T: Ord, B: HasBool> Compare<'a, T, B> for T {
    fn eq(&self, rhs: &Self) -> Bool<'a, B> {
        Bool::<B>::from(self == rhs)
    }

    fn le(&self, rhs: &Self) -> Bool<'a, B> {
        Bool::<B>::from(self <= rhs)
    }

    fn lt(&self, rhs: &Self) -> Bool<'a, B> {
        Bool::<B>::from(self < rhs)
    }
}

/// Declares that a value supports use in a backend's scope.
pub trait Scope<B: ?Sized> {}

/// Conditional branch (if-then-else).
pub trait Conditional: RustValue<bool> {
    /// Picks a value to return based on a Boolean value.
    fn conditional<T: Scope<Self>>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T;
}

/// Fixed-point loop (do-while).
pub trait FixedPoint: HasBool {
    /// Iterates on a starting state until a condition is met and returns the final state.
    fn fixed_point<'a, T: Scope<Self>>(
        &self,
        start: T,
        body: impl Fn(T) -> (Bool<'a, Self>, T),
    ) -> T;
}

pub type ExecuteBody<'a, B, I, O> =
    dyn Fn(<B as RustValue<I>>::Value<'a>) -> <B as RustValue<O>>::Value<'a> + 'a;

/// Immediately runs a function with runtime Rust data.
///
/// Used for test frameworks.
pub trait Execute: Backend {
    fn execute<I, O>(&self, func: &ExecuteBody<'_, Self, I, O>, input: I) -> O
    where
        Self: RustValue<I> + RustValue<O>;
}

macro_rules! tuple_blanket_impls {
    ($($el:ident),+) => {
        impl<T, $($el),*> RustValue<($($el,)*)> for T
        where
            $(T: RustValue<$el>),*
        {
            type Value<'a> = ($(<T as RustValue<$el>>::Value<'a>),*);
        }

        impl<T, $($el),*> Scope<T> for ($($el,)*)
        where
            $($el: Scope<T>),*
        {
        }
    };
}

macro_rules! all_tuples {
    ($el:ident) => {};
    ($head:ident, $($tail:ident),+) => {
        all_tuples!($($tail),+);
        tuple_blanket_impls!($head, $($tail),+);
    };
}

all_tuples!(A, B, C, D, E, F, G, H);

macro_rules! impl_number {
    ($name:ident, $ty:ty) => {
        #[doc = concat!("Type alias for a backend's [", stringify!($ty), "] value.")]
        pub type $name<'a, T> = <T as RustValue<$ty>>::Value<'a>;

        /// All primitives must supporting scoping.
        impl<B: Backend> Scope<B> for $ty {}
    };
}

macro_rules! impl_integers {
    () => {};
    ({ $name:ident, $number:ty } $(, { $rest_name:ident, $rest_number:ty })*) => {
        impl_number!($name, $number);
        impl_integers!($({ $rest_name, $rest_number }),*);
    };
}

impl_integers!(
    { I8, i8},
    { I16, i16},
    { I32, i32},
    { I64, i64},
    { I128, i128},
    { U8, u8},
    { U16, u16},
    { U32, u32},
    { U64, u64},
    { U128, u128}
);

macro_rules! impl_floats {
    () => {};
    ({ $name:ident, $number:ty } $(, { $rest_name:ident, $rest_number:ty })*) => {
        impl_number!($name, $number);
        impl_floats!($({ $rest_name, $rest_number }),*);
    };
}

impl_floats!(
    { F32, f32 },
    { F64, f64 }
);

macro_rules! define_has {
    ($doc:expr, $name:ident, $($bounds:ident),+) => {
        #[doc = $doc]
        pub trait $name: Sized where $(Self: RustNum<$bounds>),* {}

        impl<B> $name for B where $(Self: RustNum<$bounds>),* {}
    };
}

define_has!(
    "Backend support for all of Rust's basic integer primitives.",
    HasIntegers,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64
);

define_has!(
    "Backend support for all of Rust's basic floating-point primitives.",
    HasFloats,
    f32,
    f64
);

macro_rules! define_alias {
    ($doc:expr, $name:ident, $($bounds:ident),+) => {
        #[doc = $doc]
        pub trait $name where $(Self: $bounds),* {}

        impl<B> $name for B where $(Self: $bounds),* {}
    };
}

define_alias!(
    "Basic backend support for control flow and integers.",
    Basic,
    Conditional,
    FixedPoint,
    HasIntegers,
    HasBool
);

/// Utility enum for [RustValues](RustValue) who may be const or variable backend values.
#[derive(Copy, Clone)]
pub enum MaybeConst<C, V> {
    Const(C),
    Variable(V),
}

impl<T, V> From<T> for MaybeConst<T, V> {
    fn from(value: T) -> Self {
        Self::Const(value)
    }
}

impl<B, C, V: Scope<B>> Scope<B> for MaybeConst<C, V> {}

impl<'a, B, C, V> Compare<'a, C, B> for MaybeConst<C, V>
where
    B: HasBool,
    C: Ord,
    V: Compare<'a, C, B>,
{
    fn eq(&self, rhs: &C) -> Bool<'a, B> {
        use MaybeConst::*;
        match self {
            Const(c) => Bool::<B>::from(c == rhs),
            Variable(v) => v.eq(rhs),
        }
    }

    fn le(&self, rhs: &C) -> Bool<'a, B> {
        use MaybeConst::*;
        match self {
            Const(c) => Bool::<B>::from(c <= rhs),
            Variable(v) => v.le(rhs),
        }
    }

    fn lt(&self, rhs: &C) -> Bool<'a, B> {
        use MaybeConst::*;
        match self {
            Const(c) => Bool::<B>::from(c < rhs),
            Variable(v) => v.lt(rhs),
        }
    }
}

impl<'a, B, C, V> Compare<'a, Self, B> for MaybeConst<C, V>
where
    B: HasBool,
    Self: Compare<'a, C, B>,
    V: Compare<'a, V, B>,
{
    fn eq(&self, rhs: &Self) -> Bool<'a, B> {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(c), v) | (v, Const(c)) => v.eq(c),
            (Variable(lhs), Variable(rhs)) => lhs.eq(rhs),
        }
    }

    fn le(&self, rhs: &Self) -> Bool<'a, B> {
        use MaybeConst::*;
        match (self, rhs) {
            (lhs, Const(rhs)) => lhs.le(rhs),
            (Const(lhs), rhs) => rhs.lt(lhs).not(),
            (Variable(lhs), Variable(rhs)) => lhs.le(rhs),
        }
    }

    fn lt(&self, rhs: &Self) -> Bool<'a, B> {
        use MaybeConst::*;
        match (self, rhs) {
            (lhs, Const(rhs)) => lhs.lt(rhs),
            (Const(lhs), rhs) => rhs.le(lhs).not(),
            (Variable(lhs), Variable(rhs)) => lhs.lt(rhs),
        }
    }
}

impl<C, V> Not for MaybeConst<C, V>
where
    C: Not<Output = C>,
    V: Not<Output = V>,
{
    type Output = Self;

    fn not(self) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(c) => Const(c.not()),
            Variable(v) => Variable(v.not()),
        }
    }
}

impl<C, V> Add<C> for MaybeConst<C, V>
where
    C: Add<Output = C>,
    V: Add<C, Output = V>,
{
    type Output = Self;

    fn add(self, rhs: C) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(lhs) => Const(lhs + rhs),
            Variable(lhs) => Variable(lhs + rhs),
        }
    }
}

impl<C, V> Add for MaybeConst<C, V>
where
    Self: Add<C, Output = Self>,
    V: Add<Output = V>,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(c), v) | (v, Const(c)) => v + c,
            (Variable(lhs), Variable(rhs)) => Variable(lhs + rhs),
        }
    }
}

impl<R, C, V> AddAssign<R> for MaybeConst<C, V>
where
    Self: Add<R, Output = Self> + Copy,
{
    fn add_assign(&mut self, rhs: R) {
        *self = *self + rhs;
    }
}

impl<C, V> Mul<C> for MaybeConst<C, V>
where
    C: Mul<Output = C>,
    V: Mul<C, Output = V>,
{
    type Output = Self;

    fn mul(self, rhs: C) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(lhs) => Const(lhs * rhs),
            Variable(lhs) => Variable(lhs * rhs),
        }
    }
}

impl<C, V> Mul for MaybeConst<C, V>
where
    Self: Mul<C, Output = Self>,
    V: Mul<Output = V>,
{
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(c), v) | (v, Const(c)) => v * c,
            (Variable(lhs), Variable(rhs)) => Variable(lhs * rhs),
        }
    }
}

impl<R, C, V> MulAssign<R> for MaybeConst<C, V>
where
    Self: Mul<R, Output = Self> + Copy,
{
    fn mul_assign(&mut self, rhs: R) {
        *self = *self * rhs;
    }
}

impl<C, V> Div<C> for MaybeConst<C, V>
where
    C: Div<C, Output = C>,
    V: Div<C, Output = V>,
{
    type Output = Self;

    fn div(self, rhs: C) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(lhs) => Const(lhs / rhs),
            Variable(lhs) => Variable(lhs / rhs),
        }
    }
}

impl<C, V> Div for MaybeConst<C, V>
where
    Self: Div<C, Output = Self>,
    C: Div<V, Output = V>,
    V: Div<V, Output = V>,
{
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        use MaybeConst::*;
        match (self, rhs) {
            (lhs, Const(rhs)) => lhs / rhs,
            (Variable(lhs), Variable(rhs)) => Variable(lhs / rhs),
            (Const(lhs), Variable(rhs)) => Variable(lhs / rhs),
        }
    }
}

impl<R, C, V> DivAssign<R> for MaybeConst<C, V>
where
    Self: Div<R, Output = Self> + Copy,
{
    fn div_assign(&mut self, rhs: R) {
        *self = *self / rhs;
    }
}

impl<C, V> Sub<C> for MaybeConst<C, V>
where
    C: Sub<C, Output = C>,
    V: Sub<C, Output = V>,
{
    type Output = Self;

    fn sub(self, rhs: C) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(lhs) => Const(lhs - rhs),
            Variable(lhs) => Variable(lhs - rhs),
        }
    }
}

impl<C, V> Sub for MaybeConst<C, V>
where
    Self: Sub<C, Output = Self>,
    C: Sub<V, Output = V>,
    V: Sub<V, Output = V>,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        use MaybeConst::*;
        match (self, rhs) {
            (lhs, Const(rhs)) => lhs - rhs,
            (Variable(lhs), Variable(rhs)) => Variable(lhs - rhs),
            (Const(lhs), Variable(rhs)) => Variable(lhs - rhs),
        }
    }
}

impl<R, C, V> SubAssign<R> for MaybeConst<C, V>
where
    Self: Sub<R, Output = Self> + Copy,
{
    fn sub_assign(&mut self, rhs: R) {
        *self = *self - rhs;
    }
}

impl<C, V> Rem<C> for MaybeConst<C, V>
where
    C: Rem<C, Output = C>,
    V: Rem<C, Output = V>,
{
    type Output = Self;

    fn rem(self, rhs: C) -> Self::Output {
        use MaybeConst::*;
        match self {
            Const(lhs) => Const(lhs % rhs),
            Variable(lhs) => Variable(lhs % rhs),
        }
    }
}

impl<C, V> Rem for MaybeConst<C, V>
where
    Self: Rem<C, Output = Self>,
    C: Rem<V, Output = V>,
    V: Rem<V, Output = V>,
{
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        use MaybeConst::*;
        match (self, rhs) {
            (lhs, Const(rhs)) => lhs % rhs,
            (Variable(lhs), Variable(rhs)) => Variable(lhs % rhs),
            (Const(lhs), Variable(rhs)) => Variable(lhs % rhs),
        }
    }
}

impl<R, C, V> RemAssign<R> for MaybeConst<C, V>
where
    Self: Rem<R, Output = Self> + Copy,
{
    fn rem_assign(&mut self, rhs: R) {
        *self = *self % rhs;
    }
}
