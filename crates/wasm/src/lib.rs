use std::{
    marker::PhantomData,
    ops::{Add, Div, Mul, Not, Rem, Sub},
};

use stargazer_core::*;
use wasmi::{Val, ValType};

#[derive(Clone, Debug, Default)]
pub struct WasmBackend {}

impl Backend for WasmBackend {}

impl<T: WasmValue> RustValue<T> for WasmBackend {
    type Value<'a> = MaybeConst<T, WasmInteger<'a, T>>;
}

pub struct WasmInteger<'a, T> {
    id: usize,
    ctx: &'a BlockBuilder,
    _phantom: PhantomData<T>,
}

pub struct BlockBuilder {}

impl<'a, T> Copy for WasmInteger<'a, T> {}

pub struct FuncBuilder<'a> {
    inputs: Vec<ValType>,
    outputs: Vec<ValType>,
    block: &'a BlockBuilder,
}

impl<'a, T: WasmValue> Enter<FuncBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn enter(&self, ctx: &mut FuncBuilder<'a>) -> WasmInteger<'a, T> {
        let id = ctx.inputs.len();
        ctx.inputs.push(T::TY);

        WasmInteger {
            id,
            ctx: ctx.block,
            _phantom: PhantomData,
        }
    }
}

impl<'a, T: WasmValue> Leave<FuncBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn leave(&self, scope: &mut FuncBuilder<'a>, value: WasmInteger<'a, T>) {
        todo!("populate leave() for FuncBuilder")
    }
}

impl<'a, 'b, T: WasmValue> Enter<BlockBuilder, WasmInteger<'b, T>> for WasmBackend {
    fn enter(&self, ctx: &mut BlockBuilder) -> WasmInteger<'b, T> {
        todo!("populate enter() for BlockBuilder")
    }
}

impl<'a, 'b, T: WasmValue> Leave<BlockBuilder, WasmInteger<'b, T>> for WasmBackend {
    fn leave(&self, ctx: &mut BlockBuilder, value: WasmInteger<'b, T>) {
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
    const TY: ValType;
    type Primitive: AsWasm;
    fn pushOnTop(&self);
}

pub trait WasmValue: AsWasm {
    fn to_val(&self) -> Val;
    fn from_val(val: Val) -> Self;
}

impl AsWasm for bool {
    const TY: ValType = ValType::I32;
    type Primitive = bool;

    fn pushOnTop(&self) {
        todo!()
    }
}

impl WasmValue for bool {
    fn to_val(&self) -> Val {
        Val::I32(*self as i32)
    }

    fn from_val(val: Val) -> Self {
        val.i32().unwrap() != 0
    }
}

impl<'a, T: WasmValue> AsWasm for WasmInteger<'a, T> {
    const TY: ValType = T::TY;
    type Primitive = T;

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
    ($accessor:ident, $kind:ident,) => {};
    ($accessor:ident, $kind:ident, $head:ty $(, $tail:ty)*) => {
        impl AsWasm for $head {
            const TY: ValType = ValType::$kind;
            type Primitive = $head;

            fn pushOnTop(&self) {
                todo!()
            }
        }

        impl WasmValue for $head {
            fn to_val(&self) -> Val {
                Val::$kind(*self as _)
            }

            fn from_val(val: Val) -> Self {
                val.$accessor().unwrap() as Self
            }
        }

        impl_rhs_op!($head, Sub, sub);
        impl_rhs_op!($head, Div, div);
        impl_rhs_op!($head, Rem, rem);

        impl_as_wasm!($accessor, $kind, $($tail),*);
    };
}

impl_as_wasm!(i32, I32, u8, u16, u32, i8, i16, i32);
impl_as_wasm!(i64, I64, u64, i64);

// TODO: handle from_val() for floats
// impl_as_wasm!(f32, F32, f32);
// impl_as_wasm!(f64, F64, f64);

impl<'a, T: AsWasm> Compare<WasmInteger<'a, T::Primitive>, T> for WasmBackend {
    fn eq(&self, lhs: WasmInteger<'a, T::Primitive>, rhs: T) -> Bool<WasmBackend> {
        todo!()
    }

    fn le(&self, lhs: WasmInteger<'a, T::Primitive>, rhs: T) -> Bool<WasmBackend> {
        todo!()
    }

    fn lt(&self, lhs: WasmInteger<'a, T::Primitive>, rhs: T) -> Bool<WasmBackend> {
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
    type ConditionalScope<'a> = BlockBuilder;

    fn conditional<T>(&self, cond: Bool<Self>, if_true: T, if_false: T) -> T
    where
        Self: Scope<BlockBuilder, T>,
    {
        todo!()
    }
}

impl FixedPoint for WasmBackend {
    type FixedPointScope<'a> = BlockBuilder;

    fn fixed_point<'a, T>(&self, start: T, body: impl Fn(T) -> (Bool<'a, Self>, T)) -> T
    where
        Self: Scope<BlockBuilder, T>,
    {
        todo!()
    }
}

impl JitScopes for WasmBackend {
    type InputScope<'a> = Vec<Val>;
    type FuncScope<'a> = FuncBuilder<'a>;
    type OutputScope<'a> = Vec<Val>;
}

impl<T: WasmValue> Enter<Vec<Val>, T> for WasmBackend {
    fn enter(&self, scope: &mut Vec<Val>) -> T {
        T::from_val(scope.pop().unwrap())
    }
}

impl<T: WasmValue> Leave<Vec<Val>, T> for WasmBackend {
    fn leave(&self, scope: &mut Vec<Val>, value: T) {
        scope.push(value.to_val());
    }
}

impl Jit for WasmBackend {
    fn jit<'a, I, O>(
        &'a self,
        body: impl for<'b> JitBody<'b, Self, I, O> + 'a,
    ) -> impl FnMut(I) -> O + 'a
    where
        Self: JitEnter<'a, I> + JitLeave<'a, O>,
    {
        let block = BlockBuilder {};

        let mut func_builder = FuncBuilder {
            inputs: vec![],
            outputs: vec![],
            block: &block,
        };

        let entered = self.enter(&mut func_builder);
        let executed = body(entered);
        self.leave(&mut func_builder, executed);

        let wasm = &[];

        use wasmi::*;
        let engine = Engine::default();
        let module = Module::new(&engine, wasm).unwrap();
        let mut store = Store::new(&engine, 0u32);
        let linker = Linker::new(&engine);
        let instance = linker.instantiate_and_start(&mut store, &module).unwrap();
        let func = instance.get_func(&store, "main").unwrap();

        move |inputs| {
            let mut input_vals = Vec::new();
            self.leave(&mut input_vals, inputs);
            let mut output_vals = vec![Val::default(ValType::F64)];
            func.call(&mut store, &input_vals, &mut output_vals)
                .unwrap();
            output_vals.reverse();
            self.enter(&mut output_vals)
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
