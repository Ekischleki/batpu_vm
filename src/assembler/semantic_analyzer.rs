
use super::{access_checker, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, symbol_table::{self, BodyCode, FunctionCall, SymbolTable}, syntax::Node, type_stream::TypeStream};
///Analyzes the syntax, to check whether there are any semantic errors in them.
pub fn analyze(mut syntax_stream: TypeStream<Node>, compilation: &mut Compilation) -> SymbolTable { //This code sucks, ill make it better one day

    let mut symbol_table = SymbolTable::new();

        for current_syntax in syntax_stream {
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

    symbol_table.link(compilation);
    {
        let binding = &symbol_table.defined_functions;
        for function in binding.values() {
            check_all_func_calls(&function.borrow().body_code, compilation, &symbol_table);
            access_checker::check_access(compilation, &symbol_table, &function.borrow());
        }
    }

    symbol_table

}

fn check_all_func_calls(body_code: &Vec<Box<dyn BodyCode>>, compilation: &mut Compilation, symbol_table: &SymbolTable) {
    for code in body_code {
        let func_call = code.as_any().downcast_ref::<FunctionCall>();
        if let Some(func_call) = func_call {
            check_func_call_args(func_call, compilation, symbol_table);
        }
    }
}

fn check_func_call_args(func_call: &FunctionCall, compilation: &mut Compilation, symbol_table: &SymbolTable) {
    let reference_func_name = func_call.get_ref_func_name();
    let ref_func = symbol_table.defined_functions[reference_func_name].borrow();


    
    let func_args = ref_func.node.as_func().unwrap().2;
    let call_args = func_call.call_node.as_func_call().unwrap().1;

    if func_args != call_args {

        let location = if call_args.len() > 0 {
            call_args.first().unwrap().location().to(&call_args.last().unwrap().location())
        } else {
            func_call.call_node.get_blame_location()
        };

        compilation.add_diagnostic(
            Diagnostic::new(
                DiagnosticType::Error, 
                "The arguments of the function call don't match the ones of the function.".to_owned(), 
                Some(location),
                DiagnosticPipelineLocation::SemanticAnalysis)
        )
    }

}

