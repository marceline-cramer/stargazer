use stargazer_tests::*;

use crate::*;

// TODO: floating point arithmetic tests

#[test]
fn wasm_u32_arith() {
    let backend = WasmBackend::default();
    unsigned_arith_tests::<_, u8>(&backend);
    unsigned_arith_tests::<_, u16>(&backend);
    unsigned_arith_tests::<_, u32>(&backend);
    unsigned_arith_tests::<_, u32>(&backend);
}

#[test]
fn wasm_u64_arith() {
    let backend = WasmBackend::default();
    unsigned_arith_tests::<_, u64>(&backend);
}

#[test]
fn wasm_i32_arith() {
    let backend = WasmBackend::default();
    signed_arith_tests::<_, i8>(&backend);
    signed_arith_tests::<_, i16>(&backend);
    signed_arith_tests::<_, i32>(&backend);
    signed_arith_tests::<_, i32>(&backend);
}

#[test]
fn wasm_i64_arith() {
    let backend = WasmBackend::default();
    signed_arith_tests::<_, i64>(&backend);
}

#[test]
fn wasm_basic() {
    let backend = WasmBackend::default();
    basic_tests(&backend);
}
