use std::{cell::RefCell, collections::HashMap, ops::Deref};

use crate::assembler::token::ParamModifier;

use super::{code_location::CodeLocation, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::{InstructionSyntax, Node}, token::Token};

//This code is a pain
pub trait BodyCode {
    fn get_op(&self, symbol_table: &SymbolTable) -> Vec<Operation>;
    fn get_location(&self, symbol_table: &SymbolTable) -> CodeLocation;
    fn get_node(&self) -> &Node;
}


pub fn to_body_code(node: Node, symbol_table: &SymbolTable, compilation: &mut Compilation) -> Option<Box<dyn BodyCode>> {
    match &node {
        Node::FuncCall { identifier, .. } => {
            let call_ref_name = identifier.token_type().as_identifier().unwrap();
            let opt_ref_func = symbol_table.defined_functions.get(call_ref_name);

            if opt_ref_func.is_some() {


                let func_call = FunctionCall {
                    call_node: node,
                };

                Some(Box::from(func_call))
            } else {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("The function '{}' can't be found in this scope.", call_ref_name), 
                    Some(identifier.code_location().clone()), 
                    DiagnosticPipelineLocation::SemanticAnalysis));
                None
            }
        }

        Node::Instruction { .. } => {
            Some(Box::new(BodyInstruction {
                instruction_node: node
            }))
        }

        _ => {

            panic!("This should've been a syntax error")
        }


    }
}

#[derive(Clone, Copy)]
pub enum Operation {
    ///Reads a register, which is not possible if a register is uncerain
    ReadReg(u8),
    ///Writes to a register, making it certain
    WriteReg(u8),
    ///Masks the state of a register, making it uncertain
    Uncertify(u8)
}

impl Operation {
    pub fn get_used_reg(&self) -> u8 {
        match self {
            Self::ReadReg(a) => *a,
            Self::Uncertify(a) => *a,
            Self::WriteReg(a) => *a
        }
    }
}

pub struct BodyInstruction {
    pub instruction_node: Node,
}



impl BodyCode for BodyInstruction {
    fn get_node(&self) -> &Node {
        &self.instruction_node
    }

    fn get_op(&self, _: &SymbolTable) -> Vec<Operation> { //Keeping registers safe since 1954
            let (_, instruction_syntax) = self.instruction_node.as_instruction().unwrap();
            
            match instruction_syntax {
                InstructionSyntax::NOP => vec![],
                InstructionSyntax::HLT => vec![],
                InstructionSyntax::ADD { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    ],
                    InstructionSyntax::SUB { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    ],
                    InstructionSyntax::NOR { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    ],
                InstructionSyntax::AND { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    
                    ],
                InstructionSyntax::XOR { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    ],
                
                InstructionSyntax::RSH { a, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    ],

                InstructionSyntax::LDI { a, immediate: _ } =>             
                vec![
                    Operation::WriteReg(a.expect_register())
                    ],
                
                InstructionSyntax::ADI { a, immediate: _ } =>             
                vec![
                    Operation::ReadReg(a.expect_register()),
                    Operation::WriteReg(a.expect_register())
                    ],
                InstructionSyntax::JMP { label: _ } => vec![],
                InstructionSyntax::BRH { label: _ , condition: _} => vec![],
                InstructionSyntax::CAL { label: _ } => vec![],
                InstructionSyntax::RET => vec![],
                InstructionSyntax::LOD { a, dest, offset: _ } => 
                vec![
                    Operation::ReadReg(a.expect_register()),
                    Operation::WriteReg(dest.expect_register())
                ],
                InstructionSyntax::STR { a, source, offset: _ } => 
                vec![
                    Operation::ReadReg(a.expect_register()),
                    Operation::ReadReg(source.expect_register())
                ]
            }
        }
    
    fn get_location(&self, _: &SymbolTable) -> CodeLocation {
        let (token, _) = self.instruction_node.as_instruction().unwrap();
        return token.code_location().clone();
    }
}

pub struct FunctionCall {
    pub call_node: Node,
}

impl FunctionCall {
    pub fn get_ref_func_name<'a>(&self) -> &String {
        self.call_node.as_func_call().unwrap().0.token_type().as_identifier().unwrap()
    }
}

impl BodyCode for FunctionCall {
    fn get_node(&self) -> &Node {
        &self.call_node
    }

    fn get_op(&self, symbol_table: &SymbolTable) -> Vec<Operation> {

        let ref_func = symbol_table.defined_functions.get(self.get_ref_func_name()).unwrap().borrow();
        let func_args = ref_func.node.as_func().unwrap().2;

        let mut res = vec![];

        for arg in func_args {
            let modifier = arg.modifier.as_ref().map(|modifier| modifier.token_type().as_param_modifier().unwrap());

            if let Some(m) = modifier {
                match m {
                    ParamModifier::In => {
                        res.push(Operation::Uncertify(arg.register));
                    }
                    ParamModifier::Out => {
                        res.push(Operation::WriteReg(arg.register));
                    }
                    ParamModifier::Mut => {
                        res.push(Operation::ReadReg(arg.register));
                        res.push(Operation::WriteReg(arg.register));
                    }
                    ParamModifier::Use => {
                        res.push(Operation::Uncertify(arg.register));
                    }
                }
            } else {
                res.push(Operation::ReadReg(arg.register))
            }
        }

        res
    }


    fn get_location(&self, _: &SymbolTable) -> CodeLocation {
        let (identifier, _) = self.call_node.as_func_call().unwrap();
        identifier.code_location().clone()
    }
}

pub struct Function {
    pub node: Node,
    pub inner_labels: HashMap<String, Label>,
    pub body_code: Vec<Box<dyn BodyCode>>,

}

impl Function {
    pub fn new(node: Node) -> Self {
        Self { node, inner_labels: HashMap::new(), body_code: vec![]}
    }

   

    pub fn get_identifier(&self) -> &Token {
        self.node.as_func().unwrap().1
    }

    pub fn link_body(&mut self, symbol_table: &SymbolTable, compilation: &mut Compilation) {
        let (_, _, _, body_code) = self.node.as_func_mut().unwrap();
        for node in body_code {
            match &node.deref() {
                Node::Label { identifier, .. } => {
                    let label_name = identifier.copy_identifier();
                    let label = Label::new(self.body_code.len() as u64, *node);
                    if let Some(old_label) = self.inner_labels.get(&label_name) {
                        compilation.add_diagnostic(
                            Diagnostic::new(
                                DiagnosticType::Error, 
                                format!("The label '{}' has already been defined.", label_name), 
                                Some(label.node.get_blame_location()), 
                                DiagnosticPipelineLocation::SemanticAnalysis)
                                .with_visualisation(old_label.node.get_blame_location(), "Other instance of label with the same name is defined here".to_string())
                        )
                    }
                }
                _ => {
                    if let Some(code) = to_body_code(*node, symbol_table, compilation) {
                        self.body_code.push(Box::from(code));
                    }
                }
            }
            
        }
    }
}

pub struct Label {
    pub node: Node,
    pub instr_id: u64,
}

impl Label {
    pub fn new(next_instr_id: u64, node: Node) -> Self {
        Self { node, instr_id: next_instr_id }
    }

    pub fn name(&self) -> String {
        self.node
        .as_label()
        .unwrap()
        .1 //Identifier
        .copy_identifier()
    }
}

pub struct SymbolTable {
    pub defined_functions: HashMap<String, RefCell<Function>>,
}



impl SymbolTable {


    pub fn new() -> Self {
        Self { defined_functions: HashMap::new() }
    }

    pub fn link(&mut self, compilation: &mut Compilation) {

        let functions = self.defined_functions.values().map(|func| func.borrow_mut());

        for mut function in functions {
            function.link_body(self, compilation)
        }
    }

    pub fn try_push_func(&mut self, compilation: &mut Compilation, function_syntax: Node) {

        if let Node::Func { func_keyword: _, identifier, args: _, body: _ } = &function_syntax {
            
            let identifier_location = identifier.code_location().clone(); //For possible error reporting
            let identifier = identifier.copy_identifier();
            let function = Function::new(function_syntax);
            if let Some(old_func) = self.defined_functions.insert(identifier, RefCell::new(function)) {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("A function with the name '{}' already exists.", old_func.borrow().get_identifier().copy_identifier()), 
                    Some(identifier_location), 
                    DiagnosticPipelineLocation::SemanticAnalysis)
                    .with_visualisation(
                        old_func.borrow().get_identifier().code_location().clone(), 
                        "Other function defined here.".to_string())
                );
                
            }

        } else {
            panic!("Expected function");
        }

    }
}