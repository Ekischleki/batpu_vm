
use super::{compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, symbol_table::SymbolTable, syntax::Node, type_stream::TypeStream};
///Analyzes the syntax, to check whether there are any semantic errors in them.
pub fn analyze(mut syntax_stream: TypeStream<Node>, compilation: &mut Compilation) -> SymbolTable { //This code sucks, ill make it better one day

    let mut symbol_table = SymbolTable::new();

    loop {
        let current_syntax: Node = syntax_stream.next();

        match current_syntax {
            Node::Func { func_keyword: _, identifier: _, args: _, body: _ } => {
                symbol_table.try_push_func(compilation, current_syntax);
            }

            _ => { //This should've been a syntax error, but we'll add this as a failsave
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Unexpected {:#?} at global scope", current_syntax), 
                    Some(current_syntax.get_blame_location()), 
                    DiagnosticPipelineLocation::SemanticAnalysis));
            }
        }

    }

}