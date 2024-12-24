use std::fmt::Debug;

use super::code_location::CodeLocation;


#[derive(Debug, Clone)]
pub enum DiagnosticType {
    Info,
    Warning,
    Error
}
#[derive(Debug, Clone)]
pub enum DiagnosticPipelineLocation {
    Lexing,
    Parsing,

    SemanticAnalysis,
    Access,
    
    Internal,
    IO,
    Assembling
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub diagnostic_type: DiagnosticType,
    pub location: Option<CodeLocation>,
    pub description: String,

    pub visualisations: Vec<Visualisation>,

    pub pipeline_location: DiagnosticPipelineLocation
}

#[derive(Debug, Clone)]
pub struct Visualisation {
    pub location: CodeLocation,
    pub description: String,
}



impl Diagnostic {
    pub fn type_lower_than(&self, diagnostic_type: DiagnosticType) -> bool {
        let actual_type = Self::diagnostic_type_to_int(&self.diagnostic_type);
        let reference_type = Self::diagnostic_type_to_int(&diagnostic_type);
        return actual_type < reference_type;
    }

    fn diagnostic_type_to_int(diagnostic_type: &DiagnosticType) -> u16 {
        match diagnostic_type {
            DiagnosticType::Info => 0,
            DiagnosticType::Warning => 1,
            DiagnosticType::Error => 2,
        }
    }

    pub fn new(diagnostic_type: DiagnosticType, description: String, location: Option<CodeLocation>, pipeline_location: DiagnosticPipelineLocation) -> Self {
        Diagnostic {
            description,
            diagnostic_type,
            location,
            visualisations: vec![],
            pipeline_location
        }
    }

    pub fn with_visualisation(mut self, location: CodeLocation, description: String) -> Self {
        self.visualisations.push(Visualisation {
            location,
            description
        });

        self
    }
}