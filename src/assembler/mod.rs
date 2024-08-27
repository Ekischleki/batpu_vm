
use std::{panic, path::Path};

use assembly::AssemblyBuilder;
use compilation::Compilation;
use diagnostic::Diagnostic;
use file_reader::FileReader;
use string_file_reader::StringFileReader;

pub mod token;
pub mod code_location;
pub mod lexer;
pub mod diagnostic;
mod compilation;
pub mod file_reader;
pub mod type_stream;
pub mod syntax;
pub mod parser;
pub mod semantic_analyzer;
//pub mod symbol_table;
pub mod assembly;
pub mod string_file_reader;
#[derive(Debug)]
pub enum CompilationResult {
    Success {
        assembly: [u8; 2048],
        diagnostics: Vec<Diagnostic>
    },
    Error {
        assembly: Option<[u8; 2048]>,
        diagnostics: Vec<Diagnostic>
    },
    Crash {
        crash_message: String
    }
}

pub fn assemble(path: &Box<Path>) -> CompilationResult {
    let result = panic::catch_unwind(|| {   
        let mut compilation = Compilation::new();

        let file_reader:  &mut dyn FileReader = &mut StringFileReader::new(); //Bad file reader, will replace the default eventually.
        if let Err(diagnostic) = file_reader.reset_to_file(&path) {
            compilation.add_diagnostic(diagnostic);
            return CompilationResult::Error { assembly: None, diagnostics: compilation.diagnostics() } ;
        }

        let opt_tokens = lexer::tokenise(file_reader, &path, &mut compilation);
        let tokens;
        match opt_tokens {
            Some(t) => {
                tokens = t;
            }
            None => {
                return CompilationResult::Error { assembly: None, diagnostics: compilation.diagnostics() }
            }
        }

        

        let ast = parser::parse(&mut compilation, tokens); 
        semantic_analyzer::analyze(&ast, &mut compilation);

        let assembly_builder = AssemblyBuilder::new();
        let mut assembly = [1; 2048]; //Unused space is the HLT instruction, as a failsafe 
        match assembly_builder.build_asm(ast) {
            Ok(asm) => {
                assert!(asm.len() <= 2048);
                for (i, &val) in asm.iter().enumerate() {
                    assembly[i] = val;
                }
            }
            Err(d) => {
                compilation.add_diagnostic(d);
            }
        }

        

        if compilation.is_error_free() {
            CompilationResult::Success { assembly, diagnostics: compilation.diagnostics() }
        }
        else {
            CompilationResult::Error { assembly: Some(assembly), diagnostics: compilation.diagnostics() }
        }
    });

    match result {
        Ok(compilation_result) => compilation_result,
        Err(err) => {
            let message = if let Some(message) = err.downcast_ref::<&str>() {
                format!("Panic occurred: {}", message)
            } else if let Some(message) = err.downcast_ref::<String>() {
                format!("Panic occurred: {}", message)
            } else {
                format!("Unknown error.")
            };

            CompilationResult::Crash { crash_message: message }
        }
    }
}
