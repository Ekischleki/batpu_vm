use std::collections::HashSet;

use super::{compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, symbol_table::{self, SymbolTable}, syntax::Syntax};
///Analyzes the syntax, to check whether there are any semantic errors in them.
pub fn analyze(syntax_stream: &Vec<Syntax>, compilation: &mut Compilation) { //This code sucks, ill make it better one day

    let mut symbol_table = SymbolTable::new();


}



pub fn 