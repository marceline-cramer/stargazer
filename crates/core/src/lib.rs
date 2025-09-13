#![no_std]

use core::ops::{Add, Not};

use num::traits::{NumAssignOps, NumOps};

/// The supertrait of all backend functionality.
pub trait Backend {}

/// A backend trait associating a Rust type with some backend type.
///
/// Used for primitive numeric types (i\*, u\*, f\*).
pub trait RustValue<T>: Backend {
    type Value: Copy + Scope<Self> + Sized;
}

/// The backend supports initializing values from Rust constants.
pub trait RustConst<T>: RustValue<T, Value: From<T>> {}

/// Blanket implementation for RustConst.
impl<T, B: RustValue<T, Value: From<T>>> RustConst<T> for B {}

/// Supports comparing two Rust values and returning a Boolean.
pub trait RustCmp<T>:
    RustValue<T, Value: Compare<<Self as RustValue<T>>::Value, Self> + Compare<T, Self>> + HasBool
{
}

/// Blanket implementation for comparables.
impl<T, B> RustCmp<T> for B
where
    B: RustValue<T> + HasBool,
    <B as RustValue<T>>::Value: Compare<<B as RustValue<T>>::Value, B> + Compare<T, B>,
{
}

/// Annotates a backend's Rust value type as supporting basic numeric operations.
pub trait RustNum<T: NumOps>:
    RustCmp<T> + RustValue<T, Value: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>>
{
}

/// Blanket implementation for RustNum.
impl<T, B> RustNum<T> for B
where
    T: NumOps,
    B: RustCmp<T> + RustValue<T, Value: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>>,
{
}

/// Annotates that the Boolean type is fully supported by a backend.
pub trait HasBool: RustConst<bool, Value: Not<Output = Bool<Self>>> {}

/// Blanket implementation for HasBool.
impl<B: RustConst<bool, Value: Not<Output = Bool<Self>>>> HasBool for B {}

/// Type alias for a backend's Boolean value.
pub type Bool<T> = <T as RustValue<bool>>::Value;

/// Comparison operations for backend values.
pub trait Compare<Rhs, B: RustConst<bool> + ?Sized> {
    fn eq(&self, rhs: &Rhs) -> Bool<B>;
    fn le(&self, rhs: &Rhs) -> Bool<B>;
    fn lt(&self, rhs: &Rhs) -> Bool<B>;
}

/// Blanket implementation for `Ord` types.
impl<T: Ord, B: RustConst<bool>> Compare<T, B> for T {
    fn eq(&self, rhs: &Self) -> Bool<B> {
        Bool::<B>::from(self == rhs)
    }

    fn le(&self, rhs: &Self) -> Bool<B> {
        Bool::<B>::from(self <= rhs)
    }

    fn lt(&self, rhs: &Self) -> Bool<B> {
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
    fn fixed_point<T: Scope<Self>>(&self, start: T, body: impl Fn(T) -> (Bool<Self>, T)) -> T;
}

/// Immediately runs a function with runtime Rust data.
///
/// Used for test frameworks.
pub trait Execute: Backend {
    fn execute<I, O>(
        &self,
        func: &dyn Fn(&Self, <Self as RustValue<I>>::Value) -> <Self as RustValue<O>>::Value,
        input: I,
    ) -> O
    where
        Self: RustValue<I> + RustValue<O>;
}

macro_rules! tuple_blanket_impls {
    ($($el:ident),+) => {
        impl<T, $($el),*> RustValue<($($el,)*)> for T
        where
            $(T: RustValue<$el>),*
        {
            type Value = ($(<T as RustValue<$el>>::Value),*);
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
    ($name:ident, $ty:ty, $has:ident) => {
        /// Type alias for a backend's $ty value.
        pub type $name<T> = <T as RustValue<$ty>>::Value;

        /// All backends must accept Rust number types in scopes.
        impl<B: RustValue<$ty>> Scope<B> for $ty {}

        /// Bounds a backend to define a $ty in full.
        pub trait $has: RustConst<$ty> + RustNum<$ty> {}

        impl<B: RustConst<$ty> + RustNum<$ty>> $has for B {}
    };
}

macro_rules! impl_integers {
    () => {};
    ({ $name:ident, $number:ty, $has:ident } $(, { $rest_name:ident, $rest_number:ty, $rest_has:ident })*) => {
        impl_number!($name, $number, $has);
        impl_integers!($({ $rest_name, $rest_number, $rest_has }),*);
    };
}

impl_integers!(
    { I8, i8, HasI8},
    { I16, i16, HasI16},
    { I32, i32, HasI32},
    { I64, i64, HasI64},
    { I128, i128, HasI128},
    { U8, u8, HasU8},
    { U16, u16, HasU16},
    { U32, u32, HasU32},
    { U64, u64, HasU64},
    { U128, u128, HasU128}
);

macro_rules! impl_floats {
    () => {};
    ({ $name:ident, $number:ty, $has:ident } $(, { $rest_name:ident, $rest_number:ty, $rest_has:ident })*) => {
        impl_number!($name, $number, $has);
        impl_floats!($({ $rest_name, $rest_number, $rest_has }),*);
    };
}

impl_floats!(
    { F32, f32, HasF32 },
    { F64, f64, HasF64 }
);

macro_rules! define_alias {
    ($name:ident, $($bounds:ident),+) => {
        pub trait $name where $(Self: $bounds),* {}

        impl<B> $name for B where $(Self: $bounds),* {}
    };
}

define_alias!(
    HasIntegers,
    HasU8,
    HasU16,
    HasU32,
    HasU64,
    HasI8,
    HasI16,
    HasI32,
    HasI64
);

define_alias!(HasFloats, HasF32, HasF64);

define_alias!(Basic, Conditional, FixedPoint, HasIntegers);

/// Utility enum for RustValue values who may be const or variable backend values.
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

impl<B, C, V> Compare<Self, B> for MaybeConst<C, V>
where
    B: HasBool,
    C: Ord,
    V: Compare<V, B> + Compare<C, B>,
{
    fn eq(&self, rhs: &Self) -> Bool<B> {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(lhs), Const(rhs)) => Bool::<B>::from(lhs == rhs),
            (Const(c), Variable(v)) | (Variable(v), Const(c)) => v.eq(c),
            (Variable(lhs), Variable(rhs)) => lhs.eq(rhs),
        }
    }

    fn le(&self, rhs: &Self) -> Bool<B> {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(lhs), Const(rhs)) => Bool::<B>::from(lhs <= rhs),
            (Const(c), Variable(v)) => v.lt(c).not(),
            (Variable(v), Const(c)) => v.le(c),
            (Variable(lhs), Variable(rhs)) => lhs.le(rhs),
        }
    }

    fn lt(&self, rhs: &Self) -> Bool<B> {
        use MaybeConst::*;
        match (self, rhs) {
            (Const(lhs), Const(rhs)) => Bool::<B>::from(lhs < rhs),
            (Const(c), Variable(v)) => v.le(c).not(),
            (Variable(v), Const(c)) => v.lt(c),
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

impl<C, V> Add for MaybeConst<C, V>
where
    C: Add<Output = C> + Add<V, Output = C>,
    V: Add<Output = V> + Add<C, Output = V>,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        todo!()
    }
}
