use std::collections::HashMap;

use super::{code_location::CodeLocation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::Syntax, token::{Token, TokenType}};

pub struct SymbolTable {
    symbols: HashMap<String, Syntax> 
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }

    pub fn get_symbol(&self, identifier: &Token) -> Result<&Syntax, Diagnostic> {
        if let TokenType::Identifier(s) = identifier.token_type() {
            match self.symbols.get(s) {
                Some(s) => {
                    Ok(s)
                }
                None => {
                    return Err(
                        Diagnostic::new(
                            DiagnosticType::Error, 
                            format!("The symbol '{}' has not been defined", s), 
                            Some(identifier.code_location().clone()), 
                            DiagnosticPipelineLocation::SemanticAnalysis)
                    )
                }
            }
        } else {
            panic!("Token was not an identifier")
        }
    }

    pub fn define_symbol(&mut self, identifier: &String, symbol: Syntax) -> Option<Diagnostic> {
        match self.symbols.insert(identifier.clone(), symbol) {
            Some(s) => {
                return Some(
                    Diagnostic::new(
                        DiagnosticType::Error, 
                        format!("The symbol '{}' already has been defined", identifier), 
                        Some(Self::get_blame_identifier_location(s)), 
                        DiagnosticPipelineLocation::SemanticAnalysis)
                )
            }
            None => {
                None
            }
        }
    }

    fn get_blame_identifier_location(s: Syntax) -> CodeLocation {
        match s {
            Syntax::Define { keyword: _, identifier, value: _ } => 
                identifier.code_location().clone(),
            Syntax::Label { dot: _, identifier } => 
                identifier.code_location().clone(),
            _ => 
                panic!("{:#?} is not a syntax type that has an identifier", s)
            
        }
    }
}