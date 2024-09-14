use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use super::{symbol_table::{self, BodyCode, Function, SymbolTable}, syntax::{InstructionSyntax, Node}, token::Condition};







pub struct Compiler {
    label_counter: usize,
    function_labels: HashMap<String, usize>,
    functions: Option<Vec<Function>>
}

impl Compiler {

    pub fn new(symbol_table: SymbolTable) -> Self {
        let mut res = Self {
            label_counter: 0,
            function_labels: HashMap::new(),
            functions: Some(vec![])
        };
        
        for (name, function) in symbol_table.defined_functions.into_iter() {
            let label = res.next_label();
            res.function_labels.insert(name, label);
            res.functions.as_mut().unwrap().push(function.into_inner());
        }

        res
    }

    pub fn get_func_label(&self, function: &String) -> usize {
        self.function_labels[function]
    }

    pub fn next_label(&mut self) -> usize {
        let res = self.label_counter;
        self.label_counter += 1;
        res   
    }

    pub fn to_assembly(mut self) -> Vec<Assembly> {
        let mut res = vec![];
        
        for function in self.functions.take().expect("Object was not properly initialized") {
            let function_name = function.get_identifier().token_type().as_identifier().unwrap();
            let function_label = self.function_labels[function_name];
            res.push(Assembly::Label(function_label));

            self.compile_body_code(&function.body_code, &mut res);
        }
    
        res
    }

    fn compile_body_code(&mut self, code: &Vec<Box<dyn BodyCode>>, assembly: &mut Vec<Assembly>) {
        for body_code in code {
            body_code.as_ref().to_assembly(assembly, self);
        }
    }
}
#[derive(Debug)]
pub enum Assembly {
    Instruction(AsmInstruction),
    Label(usize)
}

#[derive(Debug)]
pub enum AsmInstruction {
    NOP,
    HLT,
    ADD{a: u8, b: u8, dest: u8},
    SUB{a: u8, b: u8, dest: u8},
    NOR{a: u8, b: u8, dest: u8},
    AND{a: u8, b: u8, dest: u8},
    XOR{a: u8, b: u8, dest: u8},
    RSH{a: u8, dest: u8},
    LDI{a: u8, immediate: u8},
    ADI{a: u8, immediate: u8},
    JMP{label: usize},
    BRH{condition: Condition, label: usize},
    CAL{label: usize},
    RET,
    LOD{a: u8, dest: u8, offset: i8},
    STR{a: u8, source: u8, offset: i8}
}