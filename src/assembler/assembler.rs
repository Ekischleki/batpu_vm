use std::collections::HashMap;

use super::{symbol_table::{self, Function, SymbolTable}, syntax::{InstructionSyntax, Node}, token::Condition};

pub fn to_assembly(symbol_table: SymbolTable) -> Vec<Assembly> {
    let function_mappings = symbol_table.defined_functions.take();
    let mut assembly = vec![];
    let mut function_labels: HashMap<String, usize> = HashMap::with_capacity(function_mappings.len());
    let mut functions = Vec::with_capacity(function_mappings.len());

    let mut label_counter: usize = 0;
    for (function_name, function) in function_mappings {
        function_labels.insert(function_name, functions.len());
        functions.push(function);
        label_counter += 1;
    }
    
    
    for i in 0..functions.len() {
        let function = &functions[i];
        assembly.push(Assembly::Label(i));

        for body_code in &function.body_code {
            let node = body_code.get_node();

            match node {
                Node::FuncCall { identifier, .. } => {
                    let ref_idx = function_labels.get(identifier.token_type().as_identifier().unwrap()).unwrap();
                    assembly.push(Assembly::Instruction(InstructionSyntax::CAL { label: () }))
                }
            }
        }
    }

    assembly
}







pub enum Assembly {
    Instruction(InstructionSyntax),
    Label(usize)
}


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