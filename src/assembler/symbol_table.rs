use std::collections::HashMap;

use crate::assembler::token::ParamModifier;

use super::{code_location::CodeLocation, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::{InstructionSyntax, Node}, token::Token};


pub trait BodyCode {
    fn get_op(&self) -> Vec<Operation>;
    fn get_location(&self) -> CodeLocation;
}
pub enum Operation {
    ///Reads a register, which is not possible if a register is uncerain
    ReadReg(u8),
    ///Writes to a register, making it certain
    WriteReg(u8),
    ///Masks the state of a register, making it uncertain
    Uncertify(u8)
}

pub struct BodyInstruction {
    pub instruction_node: Node
}

impl BodyCode for BodyInstruction {
    fn get_op(&self) -> Vec<Operation> { //Keeping registers safe since 1954
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
    fn get_location(&self) -> CodeLocation {
        let (token, _) = self.instruction_node.as_instruction().unwrap();
        return token.code_location().clone();
    }
}

pub struct FunctionCall<'a> {
    pub call_node: Node,
    pub ref_function: &'a Function<'a>,
}

impl BodyCode for FunctionCall<'_> {
    fn get_op(&self) -> Vec<Operation> {
        let mut res = vec![];
        let func_args = self.ref_function.node.as_func().unwrap().2;

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
                }
            } else {
                res.push(Operation::ReadReg(arg.register))
            }
        }

        todo!()
    }

    fn get_location(&self) -> CodeLocation {
        todo!()
    }
}

pub struct Function<'a> {
    pub node: Node,
    pub callers: Vec<&'a FunctionCall<'a>>,
    pub inner_labels: HashMap<String, Label<'a>>,
    pub id: u64,
    pub body_code: Vec<Box<dyn BodyCode>>
}

impl Function<'_> {
    pub fn new(symbol_table: &mut SymbolTable, node: Node) -> Self {
        Self { node, callers: vec![], inner_labels: HashMap::new(), id: symbol_table.get_id(), body_code: vec![] }
    }

    pub fn add_label(&mut self, symbol_table: &mut SymbolTable, label_node: Node) {
        let label = Label::new(symbol_table, label_node);
        let label_name = label.name();
        self.inner_labels.insert(label_name, label);
    }

    pub fn get_identifier(&self) -> &Token {
        self.node.as_func().unwrap().1
    }
}

pub struct Label<'a> {
    pub node: Node,
    pub callers: Vec<&'a Node>,
    pub id: u64,
}

impl Label<'_> {
    pub fn new(symbol_table: &mut SymbolTable, node: Node) -> Self {
        Self { node, callers: vec![], id: symbol_table.get_id() }
    }

    pub fn name(&self) -> String {
        self.node
        .as_label()
        .unwrap()
        .1 //Identifier
        .copy_identifier()
    }
}

pub struct SymbolTable<'a> {
    defined_functions: HashMap<String, Function<'a>>,
    id_counter: u64
}



impl SymbolTable<'_> {
    pub fn get_id(&mut self) -> u64 {
        self.id_counter += 1;
        return self.id_counter;
    }

    pub fn new() -> Self {
        Self { defined_functions: HashMap::new(), id_counter: 0 }
    }

    pub fn try_push_func(&mut self, compilation: &mut Compilation, function_syntax: Node) {

        if let Node::Func { func_keyword: _, identifier, args: _, body: _ } = &function_syntax {
            
            let identifier_location = identifier.code_location().clone(); //For possible error reporting
            let identifier = identifier.copy_identifier();
            let function = Function::new(self, function_syntax);
            if let Some(old_func) = self.defined_functions.insert(identifier, function) {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("A function with the name '{}' already exists.", old_func.get_identifier().copy_identifier()), 
                    Some(identifier_location), 
                    DiagnosticPipelineLocation::SemanticAnalysis)
                    .with_visualisation(
                        old_func.get_identifier().code_location().clone(), 
                        "Other function defined here.".to_string())
                );
                
            }

        } else {
            panic!("Expected function");
        }

    }
}