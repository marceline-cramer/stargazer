use std::{
    cell::RefCell,
    marker::PhantomData,
    ops::{Add, Div, Mul, Not, Rem, Sub},
};

use stargazer_core::*;

#[derive(Clone, Debug, Default)]
pub struct WasmBackend {}

impl Backend for WasmBackend {}

pub enum WasmScope<'a> {
    Func(&'a RefCell<FuncBuilder>),
    Block(BlockBuilder<'a>),
}

impl<T: WasmValue> RustValue<T> for WasmBackend {
    type Value<'a> = MaybeConst<T, WasmInteger<'a, T>>;
}

pub enum WasmPrimitiveKind {
    I32,
    I64,
    F32,
    F64,
}

pub struct WasmInteger<'a, T> {
    id: usize,
    ctx: &'a BlockBuilder<'a>,
    _phantom: PhantomData<T>,
}

pub struct BlockBuilder<'a> {
    backend: &'a WasmBackend,
}

impl<'a, T> Copy for WasmInteger<'a, T> {}

pub struct FuncBuilder<'a> {
    args: Vec<WasmPrimitiveKind>,
    block: &'a BlockBuilder<'a>,
}

impl<'a, T: WasmValue> Enter<FuncBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn enter(&self, ctx: &mut FuncBuilder<'a>) -> WasmInteger<'a, T> {
        let id = ctx.args.len();
        ctx.args.push(T::KIND);

        WasmInteger {
            id,
            ctx: ctx.block,
            _phantom: PhantomData,
        }
    }
}

impl<'a, T: WasmValue> Enter<BlockBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn enter(&self, ctx: &mut BlockBuilder<'a>) -> WasmInteger<'a, T> {
        let id = ctx.args.len();
        ctx.args.push(T::KIND);

        WasmInteger {
            id,
            ctx: ctx.block,
            _phantom: PhantomData,
        }
    }
}

impl<'a, 'b, T: WasmValue> Scope<BlockBuilder<'a>, WasmInteger<'b, T>> for WasmBackend {
    fn enter(&self, ctx: &mut BlockBuilder<'a>) -> WasmInteger<'b, T> {
        todo!("populate enter() for BlockBuilder")
    }

    fn leave(&self, ctx: &mut BlockBuilder<'a>, value: WasmInteger<'b, T>) {
        todo!("populate leave() for BlockBuilder")
    }
}

impl<'a, T> Clone for WasmInteger<'a, T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            ctx: self.ctx,
            _phantom: PhantomData,
        }
    }
}

impl<'a> Not for WasmInteger<'a, bool> {
    type Output = Self;

    fn not(self) -> Self::Output {
        todo!()
    }
}

pub trait AsWasm: Copy {
    type Primitive: WasmValue;
    const KIND: WasmPrimitiveKind;

    fn pushOnTop(&self);
}

pub trait WasmValue: AsWasm {}

impl WasmValue for bool {}

impl AsWasm for bool {
    type Primitive = bool;
    const KIND: WasmPrimitiveKind = WasmPrimitiveKind::I32;

    fn pushOnTop(&self) {
        todo!()
    }
}

impl<'a, T: WasmValue> AsWasm for WasmInteger<'a, T> {
    type Primitive = T;
    const KIND: WasmPrimitiveKind = T::KIND;

    fn pushOnTop(&self) {
        todo!()
    }
}

macro_rules! impl_rhs_op {
    ($prim:ty, $op:ident, $method:ident) => {
        impl<'a> $op<WasmInteger<'a, $prim>> for $prim {
            type Output = WasmInteger<'a, $prim>;

            fn $method(self, _other: WasmInteger<'a, $prim>) -> WasmInteger<'a, $prim> {
                todo!()
            }
        }
    };
}

macro_rules! impl_as_wasm {
    ($kind:expr,) => {};
    ($kind:expr, $head:ty $(, $tail:ty)*) => {
        impl WasmValue for $head {}

        impl AsWasm for $head {
            type Primitive = $head;
            const KIND: WasmPrimitiveKind = $kind;

            fn push(&self) {
                todo!()
            }
        }

        impl_rhs_op!($head, Sub, sub);
        impl_rhs_op!($head, Div, div);
        impl_rhs_op!($head, Rem, rem);

        impl_as_wasm!($kind, $($tail),*);
    };
}

impl_as_wasm!(WasmPrimitiveKind::I32, u8, u16, u32, i8, i16, i32);
impl_as_wasm!(WasmPrimitiveKind::I64, u64, i64);
impl_as_wasm!(WasmPrimitiveKind::F32, f32);
impl_as_wasm!(WasmPrimitiveKind::F64, f64);

impl<'a, T: AsWasm> Compare<'a, T, WasmBackend> for WasmInteger<'a, T> {
    fn eq(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn le(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }

    fn lt(&self, rhs: &T) -> Bool<'a, WasmBackend> {
        todo!()
    }
}

impl<'a, T: AsWasm> Add<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn add(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Mul<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Sub<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Div<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn div(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl<'a, T: AsWasm> Rem<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn rem(self, rhs: T) -> Self::Output {
        todo!()
    }
}

impl Conditional for WasmBackend {
    type ConditionalScope<'a> = BlockBuilder<'a>;

    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T
    where
        Self: for<'a> Scope<BlockBuilder<'a>, T>,
    {
        todo!()
    }
}

impl FixedPoint for WasmBackend {
    type FixedPointScope<'a> = BlockBuilder<'a>;

    fn fixed_point<'a, T>(&self, start: T, body: impl Fn(T) -> (Bool<'a, Self>, T)) -> T
    where
        Self: for<'b> Scope<BlockBuilder<'b>, T>,
    {
        todo!()
    }
}

impl Execute for WasmBackend {
    type ExecuteScope<'a> = FuncBuilder;

    fn execute<'a, I: 'a, O: 'a>(&self, func: &JitBody<'a, Self, I, O>) -> impl Fn(I) -> O + 'a
    where
        Self: RustValueScope<FuncBuilder, I> + RustValueScope<FuncBuilder, O>,
    {
        let mut builder = FuncBuilder::default();
        let entered = self.enter(&mut builder);
        let executed = (*func)(entered);
        self.leave(&mut builder, executed);

        let wasm = &[];

        use wasmi::*;
        let engine = Engine::default();
        let module = Module::new(&engine, wasm).unwrap();
        let mut store = Store::new(&engine, 0u32);
        let mut linker = Linker::new(&engine);
        let instance = linker.instantiate_and_start(&mut store, &module).unwrap();
        let func = instance.get_func(&store, "main").unwrap();

        move |inputs| {
            let mut output_vals = vec![Val::default(ValType::F64)];
            func.call(&mut store, input_vals, &mut output_vals).unwrap();
            todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use stargazer_tests::unsigned_arith_tests;

    #[test]
    fn wasm_unsigned_arith() {
        let backend = WasmBackend::default();
        unsigned_arith_tests::<u32>(&backend);
    }
}
