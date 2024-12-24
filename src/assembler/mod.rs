
use std::{panic::{self, AssertUnwindSafe}, path::PathBuf};

//use linker::AssemblyBuilder;
use compilation::Compilation;
use compiler::Compiler;
use diagnostic::Diagnostic;
use file_reader::FileReader;
use source_mapping::SourceMappings;
use string_file_reader::StringFileReader;
use type_stream::TypeStream;

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
//pub mod linker;
pub mod string_file_reader;
pub mod emulator;
pub mod source_mapping;
pub mod symbol_table;
//pub mod assembler;
pub mod access_checker;
pub mod compiler;
pub mod assembly_optimizer;
pub mod tests;
#[derive(Debug)]
pub enum CompilationResult {
    Success {
        compilation_res: ([u8; 2048], SourceMappings),
        diagnostics: Vec<Diagnostic>
    },
    Error {
        compilation_res: Option<([u8; 2048], SourceMappings)>,
        diagnostics: Vec<Diagnostic>
    },
    Crash {
        crash_message: String
    }
}


pub fn assembly_with_reader<T: FileReader>(file_reader: &mut T) -> CompilationResult {

    let result = panic::catch_unwind(AssertUnwindSafe(|| {   
        let mut compilation = Compilation::new();

        let path = if let Some(path) = file_reader.get_path() {
            path.to_owned()
        } else {
            PathBuf::new()
        };


        let opt_tokens = lexer::tokenise(file_reader, &path, &mut compilation);
        let tokens;
        match opt_tokens {
            Some(t) => {
                tokens = t;
            }
            None => {
                return CompilationResult::Error { compilation_res: None, diagnostics: compilation.diagnostics() }
            }
        }

        

        let ast = parser::parse(&mut compilation, tokens); 

        println!("{:#?}", ast);

        let symbol_table = semantic_analyzer::analyze( TypeStream::new(ast), &mut compilation);

        let compiler = Compiler::new(symbol_table);
        let assembly = compiler.to_assembly();
        print!("{:#?}", assembly);

        let assembly = assembly_optimizer::optimize(assembly); 
        print!("{:#?}", assembly);
        //let assembly_builder = AssemblyBuilder::new();
        let compilation_res =
        /* 
            match assembly_builder.build_asm(symbol_table) {
                Ok(asm) => {
                    Some(asm)
                }
                Err(d) => {
                    compilation.add_diagnostic(d);
                    None
                }
                    
                    None
            };

    */None;
        

        if compilation.is_error_free() && compilation_res.is_some() {
            CompilationResult::Success { compilation_res: compilation_res.unwrap(), diagnostics: compilation.diagnostics() }
        }
        else {
            CompilationResult::Error { compilation_res, diagnostics: compilation.diagnostics() }
        }
    }));

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


pub fn assemble(path: &PathBuf) -> CompilationResult {
    let mut file_reader = StringFileReader::new();
    file_reader.reset_to_file(path);

    assembly_with_reader(&mut file_reader)
}
