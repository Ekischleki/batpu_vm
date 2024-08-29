use std::{path::{Path, PathBuf}, str::FromStr};

use assembler::CompilationResult;
use vm::cpu::CPU;

pub mod vm;
pub mod assembler;

fn main() {
    let compiler_result = assembler::assemble(&PathBuf::from_str("src/assembler_test.txt").unwrap());
    println!("{:#?}", compiler_result);

    if let CompilationResult::Success { compilation_res, diagnostics: _ } = compiler_result {
        let mut cpu = CPU::with_rom(compilation_res.0);
        cpu.run();
    }
}


enum MyEnum {
    VariantOne(i32),
    VariantTwo { value_one: u32, value_two: String },
}