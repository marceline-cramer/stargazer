#![no_std]

use core::ops::{
    Add, AddAssign, Div, DivAssign, Mul, MulAssign, Not, Rem, RemAssign, Sub, SubAssign,
};

use num::{
    Num,
    traits::{NumAssignOps, NumOps},
};

/// The supertrait of all backend functionality.
pub trait Backend {}

/// Implements a type entering a scope.
pub trait Enter<S, T>: Backend {
    /// With the scope as context, create a value.
    fn enter(&self, scope: &mut S) -> T;
}

/// Implements a type leaving a scope.
pub trait Leave<S, T>: Backend {
    /// Exit the type from scope.
    fn leave(&self, scope: &mut S, value: T);
}

/// Defines full scoping operations for a type.
pub trait Scope<S, T>: Enter<S, T> + Leave<S, T> {}
impl<B, S, T> Scope<S, T> for B where B: Enter<S, T> + Leave<S, T> {}

/// Conditional branch (if-then-else).
pub trait Conditional: RustValue<bool> {
    /// The type to use for conditional scopes.
    type ConditionalScope<'a>;

    /// Picks a value to return based on a Boolean value.
    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T
    where
        Self: for<'a> Scope<Self::ConditionalScope<'a>, T>;
}

/// Fixed-point loop (do-while).
pub trait FixedPoint: RustValue<bool> {
    /// The type to use for fixed-point scopes.
    type FixedPointScope<'a>;

    /// Iterates on a starting state until a condition is met and returns the final state.
    fn fixed_point<'a, T>(&'a self, start: T, body: impl Fn(T) -> (Bool<'a, Self>, T)) -> T
    where
        Self: for<'b> Scope<Self::FixedPointScope<'b>, T>;
}

/// A backend trait associating a Rust type with some backend value type.
pub trait RustValue<T>: Backend {
    type Value<'a>: Copy;
}

/// The backend supports initializing values from Rust constants.
pub trait RustConst<T>: for<'a> RustValue<T, Value<'a>: From<T>> {}
impl<T, B: RustValue<T>> RustConst<T> for B where for<'a> B::Value<'a>: From<T> {}

/// Annotates that the Boolean type is fully supported by a backend.
pub trait HasBool:
    for<'a> RustConst<bool, Value<'a>: SelfNot>
    + for<'a> JitEnter<'a, bool>
    + for<'a> JitLeave<'a, bool>
{
}

impl<B: RustConst<bool>> HasBool for B
where
    for<'a> B::Value<'a>: SelfNot,
    B: for<'a> JitEnter<'a, bool> + for<'a> JitLeave<'a, bool>,
{
}

/// Blanket trait for types whose [Not] implementation is themselves.
///
/// This is its own trait because otherwise, [HasBool] would require an
/// associated type bound on Self::Value that is not supported by Rust.
pub trait SelfNot: Not<Output = Self> {}
impl<T: Not<Output = T>> SelfNot for T {}

/// Implements comparison between two types.
pub trait Compare<'a, L: 'a, R: 'a = L>: RustValue<bool> {
    fn eq(&self, lhs: L, rhs: R) -> Bool<'a, Self>;
    fn le(&self, lhs: L, rhs: R) -> Bool<'a, Self>;
    fn lt(&self, lhs: L, rhs: R) -> Bool<'a, Self>;
}

/// Blanket implementation for `Ord` types.
impl<'a, T: Ord + 'a, B: RustConst<bool>> Compare<'a, T, T> for B {
    fn eq(&self, lhs: T, rhs: T) -> Bool<'a, B> {
        Bool::<B>::from(lhs == rhs)
    }

    fn le(&self, lhs: T, rhs: T) -> Bool<'a, B> {
        Bool::<B>::from(lhs <= rhs)
    }

    fn lt(&self, lhs: T, rhs: T) -> Bool<'a, B> {
        Bool::<B>::from(lhs < rhs)
    }
}

/// Support comparing a Rust type against its backend value.
pub trait RustCompare<T>:
    RustValue<T>
    + for<'a> Compare<'a, <Self as RustValue<T>>::Value<'a>>
    + for<'a> Compare<'a, <Self as RustValue<T>>::Value<'a>, T>
{
}

impl<B, T> RustCompare<T> for B where
    B: RustValue<T>
        + for<'a> Compare<'a, T, T>
        + for<'a> Compare<'a, <Self as RustValue<T>>::Value<'a>>
        + for<'a> Compare<'a, <Self as RustValue<T>>::Value<'a>, T>
{
}

/// Annotates a backend's Rust value type as supporting basic numeric operations.
pub trait RustNumOps<T: NumOps>:
    for<'a> RustValue<T, Value<'a>: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>>
{
}

impl<B, T: NumOps> RustNumOps<T> for B where
    B: for<'a> RustValue<T, Value<'a>: NumOps + NumAssignOps + NumOps<T> + NumAssignOps<T>>
{
}

/// Blanket trait for types that support basic control flow operations.
pub trait ControlFlow<T>:
    Conditional
    + FixedPoint
    + for<'a> Scope<Self::ConditionalScope<'a>, T>
    + for<'a> Scope<Self::FixedPointScope<'a>, T>
{
}

impl<B, T> ControlFlow<T> for B where
    B: Conditional
        + FixedPoint
        + for<'a> Scope<Self::ConditionalScope<'a>, T>
        + for<'a> Scope<Self::FixedPointScope<'a>, T>
{
}

/// Blanket trait for Rust values that support basic control flow operations.
pub trait RustControlFlow<T>:
    RustValue<T> + for<'a> ControlFlow<<Self as RustValue<T>>::Value<'a>>
{
}

impl<B, T> RustControlFlow<T> for B where
    B: RustValue<T> + for<'a> ControlFlow<<Self as RustValue<T>>::Value<'a>>
{
}

/// This backend fully supports a number type.
pub trait RustNum<T: Num>:
    RustCompare<T>
    + RustNumOps<T>
    + RustConst<T>
    + for<'a> JitEnter<'a, T>
    + for<'a> JitLeave<'a, T>
    + RustControlFlow<T>
{
}

impl<B, T: Num> RustNum<T> for B where
    B: RustCompare<T>
        + RustNumOps<T>
        + RustConst<T>
        + for<'a> JitEnter<'a, T>
        + for<'a> JitLeave<'a, T>
        + RustControlFlow<T>
{
}

/// Type alias for a backend's Boolean value.
pub type Bool<'a, T> = <T as RustValue<bool>>::Value<'a>;

pub trait RustValueEnter<'a, S, T>:
    RustValue<T> + Enter<S, <Self as RustValue<T>>::Value<'a>>
{
}

impl<'a, B, S, T> RustValueEnter<'a, S, T> for B where
    B: RustValue<T> + Enter<S, <Self as RustValue<T>>::Value<'a>>
{
}

pub trait RustValueLeave<'a, S, T>:
    RustValue<T> + Leave<S, <Self as RustValue<T>>::Value<'a>>
{
}

impl<'a, B, S, T> RustValueLeave<'a, S, T> for B where
    B: RustValue<T> + Leave<S, <Self as RustValue<T>>::Value<'a>>
{
}

pub trait JitScopes: Backend {
    type InputScope<'a>;
    type FuncScope<'a>;
    type OutputScope<'a>;
}

pub trait JitEnter<'a, T>:
    JitScopes + Leave<Self::InputScope<'a>, T> + for<'b> RustValueEnter<'b, Self::FuncScope<'b>, T>
{
}

impl<'a, B, T> JitEnter<'a, T> for B where
    B: JitScopes
        + Leave<Self::InputScope<'a>, T>
        + for<'b> RustValueEnter<'b, Self::FuncScope<'b>, T>
{
}

pub trait JitLeave<'a, T>:
    JitScopes + for<'b> RustValueLeave<'b, Self::FuncScope<'b>, T> + Enter<Self::OutputScope<'a>, T>
{
}

impl<'a, B, T> JitLeave<'a, T> for B where
    B: JitScopes
        + for<'b> RustValueLeave<'b, Self::FuncScope<'b>, T>
        + Enter<Self::OutputScope<'a>, T>
{
}

pub trait JitBody<'a, B: 'a, I, O>:
    Fn(&'a B, <B as RustValue<I>>::Value<'a>) -> <B as RustValue<O>>::Value<'a>
where
    B: RustValue<I> + RustValue<O> + ?Sized,
{
}

impl<'a, F, B: 'a, I, O> JitBody<'a, B, I, O> for F
where
    F: Fn(&'a B, <B as RustValue<I>>::Value<'a>) -> <B as RustValue<O>>::Value<'a>,
    B: RustValue<I> + RustValue<O> + ?Sized,
{
}

pub trait Jit: JitScopes {
    fn jit<'a, I, O>(
        &'a self,
        body: impl for<'b> JitBody<'b, Self, I, O> + 'a,
    ) -> impl FnMut(I) -> O + 'a
    where
        Self: JitEnter<'a, I> + JitLeave<'a, O>;
}

macro_rules! tuple_blanket_impls {
    ($($el:ident),+) => {
        impl<T, $($el),*> RustValue<($($el,)*)> for T
        where
            $(T: RustValue<$el>,)*
        {
            type Value<'a> = ($(<T as RustValue<$el>>::Value<'a>),*);
        }

        impl<Ctx, T, $($el),*> Enter<Ctx, ($($el,)*)> for T
        where
            $(T: Enter<Ctx, $el>,)*
        {
            #[allow(non_snake_case)]
            fn enter(&self, ctx: &mut Ctx) -> ($($el,)*) {
                ($(<T as Enter<Ctx, $el>>::enter(self, ctx),)*)
            }
        }

        impl<Ctx, T, $($el),*> Leave<Ctx, ($($el,)*)> for T
        where
            $(T: Leave<Ctx, $el>,)*
        {
            #[allow(non_snake_case)]
            fn leave(&self, ctx: &mut Ctx, val: ($($el,)*)) {
                let ($($el,)*) = val;
                $(self.leave(ctx, $el));*
            }
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

impl<Ctx, B: Enter<Ctx, V>, C, V> Enter<Ctx, MaybeConst<C, V>> for B {
    fn enter(&self, ctx: &mut Ctx) -> MaybeConst<C, V> {
        MaybeConst::Variable(self.enter(ctx))
    }
}

impl<Ctx, B: Leave<Ctx, V>, C, V> Leave<Ctx, MaybeConst<C, V>> for B {
    fn leave(&self, ctx: &mut Ctx, value: MaybeConst<C, V>) {
        use MaybeConst::*;
        match value {
            Const(_) => {}
            Variable(v) => self.leave(ctx, v),
        }
    }
}

impl<T, V> From<T> for MaybeConst<T, V> {
    fn from(value: T) -> Self {
        Self::Const(value)
    }
}

impl<'a, B, C, V> Compare<'a, MaybeConst<C, V>, C> for B
where
    C: 'a,
    V: 'a,
    B: Compare<'a, V, C> + Compare<'a, C, C>,
{
    fn eq(&self, lhs: MaybeConst<C, V>, rhs: C) -> Bool<'a, B> {
        use MaybeConst::*;
        match lhs {
            Const(c) => self.eq(c, rhs),
            Variable(v) => self.eq(v, rhs),
        }
    }

    fn le(&self, lhs: MaybeConst<C, V>, rhs: C) -> Bool<'a, B> {
        use MaybeConst::*;
        match lhs {
            Const(c) => self.le(c, rhs),
            Variable(v) => self.le(v, rhs),
        }
    }

    fn lt(&self, lhs: MaybeConst<C, V>, rhs: C) -> Bool<'a, B> {
        use MaybeConst::*;
        match lhs {
            Const(c) => self.lt(c, rhs),
            Variable(v) => self.lt(v, rhs),
        }
    }
}

impl<'a, B, C, V> Compare<'a, MaybeConst<C, V>, MaybeConst<C, V>> for B
where
    C: 'a,
    V: 'a,
    B: Compare<'a, MaybeConst<C, V>, C> + Compare<'a, V, V> + HasBool,
{
    fn eq(&self, lhs: MaybeConst<C, V>, rhs: MaybeConst<C, V>) -> Bool<'a, B> {
        use MaybeConst::*;
        match (lhs, rhs) {
            (Const(c), v) | (v, Const(c)) => self.eq(v, c),
            (Variable(lhs), Variable(rhs)) => self.eq(lhs, rhs),
        }
    }

    fn le(&self, lhs: MaybeConst<C, V>, rhs: MaybeConst<C, V>) -> Bool<'a, B> {
        use MaybeConst::*;
        match (lhs, rhs) {
            (lhs, Const(rhs)) => self.le(lhs, rhs),
            (Const(lhs), rhs) => self.lt(rhs, lhs).not(),
            (Variable(lhs), Variable(rhs)) => self.le(lhs, rhs),
        }
    }

    fn lt(&self, lhs: MaybeConst<C, V>, rhs: MaybeConst<C, V>) -> Bool<'a, B> {
        use MaybeConst::*;
        match (lhs, rhs) {
            (lhs, Const(rhs)) => self.lt(lhs, rhs),
            (Const(lhs), rhs) => self.le(rhs, lhs).not(),
            (Variable(lhs), Variable(rhs)) => self.lt(lhs, rhs),
        }
    }
}

impl<C: SelfNot, V: SelfNot> Not for MaybeConst<C, V> {
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
