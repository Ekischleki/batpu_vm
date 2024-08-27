use std::collections::HashSet;

use super::{compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::Syntax};
///Analyzes the syntax, to check whether there are any semantic errors in them.
pub fn analyze(syntax_stream: &Vec<Syntax>, compilation: &mut Compilation) { //This code sucks, ill make it better one day
    let mut defined_labels = HashSet::new();

    for syntax in syntax_stream {
        match &syntax {
            Syntax::Label { dot: _, identifier } => {
                if !defined_labels.insert(identifier.clone().unwrap_identifier()) {
                    compilation.add_diagnostic(Diagnostic::new(
                        DiagnosticType::Error, 
                        format!("A label with the name '{}' already exists.", identifier.clone().unwrap_identifier()), 
                        Some(identifier.code_location().clone()), 
                        DiagnosticPipelineLocation::SemanticAnalysis))
                }
            },
            _ => {}
        }
    }
    for syntax in syntax_stream {
        match &syntax {
            Syntax::Instruction { original_instruction, instruction_syntax } => {
                if let Some(s) = instruction_syntax.get_label() {
                    if !defined_labels.contains(s) {
                        compilation.add_diagnostic(Diagnostic::new(
                            DiagnosticType::Error, 
                            format!("The label '{}' doesn't exist.", s), 
                            Some(original_instruction.code_location().clone()), 
                            DiagnosticPipelineLocation::SemanticAnalysis))
                    }
                }
                
            }
            _ => {}
        }
    }

}