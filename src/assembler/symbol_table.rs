use std::collections::HashMap;

use super::syntax::Syntax;

pub struct SymbolTable {
    defined_methods: HashMap<String, Syntax>
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { defined_methods: HashMap::new() }
    }
}