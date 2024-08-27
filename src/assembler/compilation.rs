use super::diagnostic::{Diagnostic, DiagnosticType};


pub struct Compilation {
    diagnostics: Vec<Diagnostic>
}


impl Compilation {
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn is_error_free(&self) -> bool {
        self.diagnostics.iter().all(|d|  d.type_lower_than(DiagnosticType::Error))
    }


    pub fn new() -> Self {
        Self {
            diagnostics: vec![]
        }
    }
    
    pub fn diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}