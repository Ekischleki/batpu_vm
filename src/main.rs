use std::path::Path;

use assembler::CompilationResult;
use vm::cpu::CPU;

pub mod vm;
pub mod assembler;

fn main() {
    let compiler_result = assembler::assemble(&Box::from(Path::new("assembler_test.txt")));
    println!("{:#?}", compiler_result);

    if let CompilationResult::Success { assembly, diagnostics: _ } = compiler_result {
        let mut cpu = CPU::with_rom(assembly);
        cpu.run();
    }
}
