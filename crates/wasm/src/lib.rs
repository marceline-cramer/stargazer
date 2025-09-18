use std::{
    cell::RefCell,
    fmt::{Display, Formatter, Write},
    marker::PhantomData,
    ops::{Add, Div, Mul, Not, Rem, Sub},
};

use stargazer_core::*;
use wasmi::Val;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Default)]
pub struct WasmBackend {}

impl Backend for WasmBackend {}

impl Jit for WasmBackend {
    fn jit<'a, I, O>(
        &'a self,
        body: impl for<'b> JitBody<'b, Self, I, O> + 'a,
    ) -> impl FnMut(I) -> O + 'a
    where
        Self: JitEnter<'a, I> + JitLeave<'a, O>,
    {
        let ctx = RefCell::new(Context::default());
        let block = BlockBuilder { ctx: &ctx };

        let mut func_builder = FuncBuilder {
            inputs: vec![],
            outputs: vec![],
            block: &block,
        };

        let entered = self.enter(&mut func_builder);
        let executed = body(self, entered);
        self.leave(&mut func_builder, executed);

        let mut wasm = String::new();
        func_builder.to_module("main", &mut wasm).unwrap();
        println!("{wasm}");

        use wasmi::*;
        let engine = Engine::default();
        let module = Module::new(&engine, wasm.as_bytes()).unwrap();
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

impl JitScopes for WasmBackend {
    type InputScope<'a> = Vec<Val>;
    type FuncScope<'a> = FuncBuilder<'a>;
    type OutputScope<'a> = Vec<Val>;
}

impl<T: WasmValue> RustValue<T> for WasmBackend {
    type Value<'a> = MaybeConst<T, WasmInteger<'a, T>>;
}

pub struct WasmInteger<'a, T> {
    id: usize,
    block: &'a BlockBuilder<'a>,
    _phantom: PhantomData<T>,
}

impl<'a, T> Clone for WasmInteger<'a, T> {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            block: self.block,
            _phantom: PhantomData,
        }
    }
}

impl<'a> Not for WasmInteger<'a, bool> {
    type Output = Self;

    fn not(self) -> Self::Output {
        self.push_on_top(self.block);
        self.block.emit("i32.eqz", "not");
        self.block.pop_value()
    }
}

impl<'a, T: WasmValue> AsWasm for WasmInteger<'a, T> {
    const TY: WasmPrimitive = T::TY;
    const SIGNED: bool = T::SIGNED;
    type Primitive = T;

    fn push_on_top(&self, block: &BlockBuilder) {
        assert!(core::ptr::eq(block, self.block));

        block.emit(
            format!("local.get ${}", self.id),
            format!("push WasmInteger<{}>", T::TY),
        );
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

impl<'a, T: WasmValue> WasmInteger<'a, T> {
    pub fn binary_op<Rhs: AsWasm<Primitive = T>>(
        &self,
        rhs: Rhs,
        signed: &str,
        unsigned: &str,
    ) -> WasmInteger<'a, T> {
        self.push_on_top(self.block);
        rhs.push_on_top(self.block);

        let op = if T::SIGNED { signed } else { unsigned };

        self.block
            .emit(format!("{}.{op}", T::TY), format!("{op}<{}>", T::TY));

        self.block.pop_value()
    }
}

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
        self.binary_op(rhs, "add", "add")
    }
}

impl<'a, T: AsWasm> Mul<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        self.binary_op(rhs, "mul", "mul")
    }
}

impl<'a, T: AsWasm> Sub<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn sub(self, rhs: T) -> Self::Output {
        self.binary_op(rhs, "sub", "sub")
    }
}

impl<'a, T: AsWasm> Div<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn div(self, rhs: T) -> Self::Output {
        self.binary_op(rhs, "div_s", "div_u")
    }
}

impl<'a, T: AsWasm> Rem<T> for WasmInteger<'a, T::Primitive> {
    type Output = Self;

    fn rem(self, rhs: T) -> Self::Output {
        self.binary_op(rhs, "rem_s", "rem_u")
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
        Self: Scope<BlockBuilder<'a>, T>,
    {
        todo!()
    }
}

pub struct FuncBuilder<'a> {
    inputs: Vec<WasmPrimitive>,
    outputs: Vec<WasmPrimitive>,
    block: &'a BlockBuilder<'a>,
}

impl<'a> FuncBuilder<'a> {
    pub fn to_module(&self, name: &str, output: &mut impl Write) -> std::fmt::Result {
        writeln!(output, "(module")?;
        writeln!(output, "  (export \"{name}\" (func ${name}))",)?;

        writeln!(output, "  (func ${name}")?;

        for (id, ty) in self.inputs.iter().enumerate() {
            writeln!(output, "    (param ${id} {ty})")?;
        }

        for ty in &self.outputs {
            writeln!(output, "    (result {ty})")?;
        }

        self.block.ctx.borrow().build(output, "    ")?;

        writeln!(output, "  )")?;
        writeln!(output, ")")?;

        Ok(())
    }
}

impl<'a, T: WasmValue> Enter<FuncBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn enter(&self, func: &mut FuncBuilder<'a>) -> WasmInteger<'a, T> {
        let id = func.inputs.len();
        func.inputs.push(T::TY);

        WasmInteger {
            id,
            block: func.block,
            _phantom: PhantomData,
        }
    }
}

impl<'a, T: WasmValue> Leave<FuncBuilder<'a>, WasmInteger<'a, T>> for WasmBackend {
    fn leave(&self, func: &mut FuncBuilder<'a>, value: WasmInteger<'a, T>) {
        func.outputs.push(T::TY);
        value.push_on_top(func.block);
    }
}

#[derive(Clone, Debug)]
pub struct BlockBuilder<'a> {
    ctx: &'a RefCell<Context>,
}

impl<'a> BlockBuilder<'a> {
    pub fn emit(&self, instr: impl ToString, comment: impl ToString) {
        self.ctx.borrow_mut().emit(instr, comment);
    }

    pub fn pop_value<T: WasmValue>(&self) -> WasmInteger<'_, T> {
        let mut ctx = self.ctx.borrow_mut();
        let id = ctx.locals.len();
        ctx.locals.push(T::TY);
        drop(ctx); // do not borrow context again

        self.emit(format!("local.set ${id}"), format!("pop_value<{}>", T::TY));

        WasmInteger {
            id,
            block: self,
            _phantom: PhantomData,
        }
    }
}

impl<'a, 'b, T: WasmValue> Enter<BlockBuilder<'b>, WasmInteger<'a, T>> for WasmBackend {
    fn enter(&self, block: &mut BlockBuilder<'b>) -> WasmInteger<'a, T> {
        todo!("populate enter() for BlockBuilder")
    }
}

impl<'a, 'b, T: WasmValue> Leave<BlockBuilder<'b>, WasmInteger<'a, T>> for WasmBackend {
    fn leave(&self, block: &mut BlockBuilder<'b>, value: WasmInteger<'a, T>) {
        todo!("populate leave() for BlockBuilder")
    }
}

impl<'a, T> Copy for WasmInteger<'a, T> {}

#[derive(Clone, Debug, Default)]
pub struct Context {
    locals: Vec<WasmPrimitive>,
    instructions: Vec<(String, String)>,
}

impl Context {
    pub fn emit(&mut self, instr: impl ToString, comment: impl ToString) {
        self.instructions
            .push((instr.to_string(), comment.to_string()));
    }

    pub fn build(&self, output: &mut impl Write, prefix: &str) -> std::fmt::Result {
        let max_width = self
            .instructions
            .iter()
            .map(|(instr, _)| instr.len())
            .max()
            .unwrap_or(0);

        for (instr, comment) in self.instructions.iter() {
            if comment.is_empty() {
                writeln!(output, "{prefix}{}", instr)?;
                continue;
            }

            writeln!(
                output,
                "{prefix}{:<width$} ;; {}",
                instr,
                comment,
                width = max_width + 1
            )?;
        }

        Ok(())
    }
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

pub trait AsWasm: Copy {
    const TY: WasmPrimitive;
    // TODO: move to a dedicated numeric trait?
    const SIGNED: bool;
    type Primitive: WasmValue;
    fn push_on_top(&self, block: &BlockBuilder);
}

pub trait WasmValue: AsWasm {
    fn to_val(&self) -> Val;
    fn from_val(val: Val) -> Self;
}

macro_rules! impl_as_wasm {
    ($accessor:ident, $kind:ident, $signed:expr,) => {};
    ($accessor:ident, $kind:ident, $signed:expr, $head:ty $(, $tail:ty)*) => {
        impl AsWasm for $head {
            const TY: WasmPrimitive = WasmPrimitive::$kind;
            const SIGNED: bool = $signed;
            type Primitive = $head;

            fn push_on_top(&self, block: &BlockBuilder) {
                block.emit(format!("{}.const {}", Self::TY, self), self);
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

        impl_as_wasm!($accessor, $kind, $signed, $($tail),*);
    };
}

impl_as_wasm!(i32, I32, false, u8, u16, u32);
impl_as_wasm!(i32, I32, true, i8, i16, i32);
impl_as_wasm!(i64, I64, false, u64);
impl_as_wasm!(i64, I64, true, i64);

// TODO: handle from_val() for floats
// impl_as_wasm!(f32, F32, f32);
// impl_as_wasm!(f64, F64, f64);

impl AsWasm for bool {
    const TY: WasmPrimitive = WasmPrimitive::I32;
    const SIGNED: bool = false;
    type Primitive = bool;

    fn push_on_top(&self, block: &BlockBuilder) {
        let val = if *self { 1 } else { 0 };
        block.emit(format!("i32.const {val}"), self);
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum WasmPrimitive {
    I32,
    I64,
    F32,
    F64,
}

impl From<WasmPrimitive> for wasmi::ValType {
    fn from(prim: WasmPrimitive) -> Self {
        use wasmi::ValType::*;
        match prim {
            WasmPrimitive::I32 => I32,
            WasmPrimitive::I64 => I64,
            WasmPrimitive::F32 => F32,
            WasmPrimitive::F64 => F64,
        }
    }
}

impl From<wasmi::ValType> for WasmPrimitive {
    fn from(value: wasmi::ValType) -> Self {
        use wasmi::ValType::*;
        match value {
            I32 => WasmPrimitive::I32,
            I64 => WasmPrimitive::I64,
            F32 => WasmPrimitive::F32,
            F64 => WasmPrimitive::F64,
            _ => unimplemented!("unsupported value type"),
        }
    }
}

impl Display for WasmPrimitive {
    fn fmt(&self, w: &mut Formatter) -> std::fmt::Result {
        use WasmPrimitive::*;
        let name = match self {
            I32 => "i32",
            I64 => "i64",
            F32 => "f32",
            F64 => "f64",
        };

        write!(w, "{name}")
    }
}
