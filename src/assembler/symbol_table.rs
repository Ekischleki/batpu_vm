use std::{any::Any, cell::RefCell, collections::HashMap};

use crate::assembler::token::ParamModifier;

use super::{code_location::CodeLocation, compilation::Compilation, compiler::{AsmInstruction, Assembly, Compiler}, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::{InstructionSyntax, Node}, token::{Condition, Token}, type_stream::TypeStream};

//This code is a pain
pub trait BodyCode : Any {
    fn get_op(&self, symbol_table: &SymbolTable) -> Vec<Operation>;
    fn get_location(&self, symbol_table: &SymbolTable) -> CodeLocation;
    fn get_node(&self) -> &Node;
    fn as_any(&self) -> &dyn Any;
    fn to_assembly(&self, assembly: &mut Vec<Assembly>, compiler: &mut Compiler);
}


pub fn to_body_code(mut node: Node, symbol_table: &SymbolTable, compilation: &mut Compilation) -> Option<Box<dyn BodyCode>> {
    match &mut node {
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

        Node::If { body, condition, .. } => {
            Some(Box::new(BranchInstruction {
                    branches: vec![ (condition.token_type().as_condition().unwrap().to_owned(), into_body_code(symbol_table, compilation, body)) ],
                    node,
                    last_is_guaranteed: false
                }))
        }

        Node::IfElse { if_body, else_body, condition, .. } => {
            Some(Box::new(BranchInstruction {
                branches: vec![ (condition.token_type().as_condition().unwrap().to_owned(), into_body_code(symbol_table, compilation, if_body)), (Condition::EQ, into_body_code(symbol_table, compilation, else_body)) ],
                node,
                last_is_guaranteed: true
            }))
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
    fn as_any(&self) -> &dyn Any {
        self
    }
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
    
    fn to_assembly(&self, assembly: &mut Vec<Assembly>, _compiler: &mut Compiler) {
        let (_, instruction_syntax) = self.instruction_node.as_instruction().unwrap();

        let instruction = match instruction_syntax {
            InstructionSyntax::ADD { a, b, dest } => 
                AsmInstruction::ADD { a: a.expect_register(), b: b.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::SUB { a, b, dest } =>
                AsmInstruction::SUB { a: a.expect_register(), b: b.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::NOR { a, b, dest } =>
                AsmInstruction::NOR { a: a.expect_register(), b: b.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::AND { a, b, dest } =>
                AsmInstruction::AND { a: a.expect_register(), b: b.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::XOR { a, b, dest } =>
                AsmInstruction::AND { a: a.expect_register(), b: b.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::RSH { a, dest } =>
                AsmInstruction::RSH { a: a.expect_register(), dest: dest.expect_register() },

            InstructionSyntax::LDI { a, immediate } => 
                AsmInstruction::LDI { a: a.expect_register(), immediate: immediate.expect_constant() },

            InstructionSyntax::ADI { a, immediate } => 
                AsmInstruction::ADI { a: a.expect_register(), immediate: immediate.expect_constant() },

            InstructionSyntax::RET => AsmInstruction::RET,

            InstructionSyntax::NOP => AsmInstruction::NOP,

            InstructionSyntax::HLT => AsmInstruction::HLT,

            InstructionSyntax::LOD { a, dest, offset } => AsmInstruction::LOD { a: a.expect_register(), dest: dest.expect_register(), offset: offset.expect_constant() as i8 },

            InstructionSyntax::STR { a, source, offset } => AsmInstruction::STR { a: a.expect_register(), source: source.expect_register(), offset: offset.expect_constant() as i8 },


        };

        assembly.push(Assembly::Instruction(instruction));
    
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
    fn to_assembly(&self, assembly: &mut Vec<Assembly>, compiler: &mut Compiler) {
        let label = compiler.get_func_label(self.call_node.as_func_call().unwrap().0.token_type().as_identifier().unwrap());

        assembly.push(
            Assembly::Instruction(AsmInstruction::CAL { label })
        );
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
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
    pub body_code: Vec<Box<dyn BodyCode>>,
}

impl Function {
    pub fn new(node: Node) -> Self {
        Self { node, body_code: vec![]}
    }

   

    pub fn get_identifier(&self) -> &Token {
        self.node.as_func().unwrap().1
    }

    pub fn link_body(&mut self, symbol_table: &SymbolTable, compilation: &mut Compilation) {
        let (_, _, _, body_code) = self.node.as_func_mut().unwrap();
        let body_code = into_body_code(symbol_table, compilation, body_code);
        self.body_code = body_code;
    }
}

fn into_body_code(symbol_table: &SymbolTable, compilation: &mut Compilation, body_code: &mut TypeStream<Box<Node>> ) -> Vec<Box<dyn BodyCode>> {
    let mut res = vec![];
    for node in body_code {
        if let Some(code) = to_body_code(*node, symbol_table, compilation) {
            res.push(Box::from(code));
        }
    }
    res
}
pub struct BranchInstruction {
    pub node: Node,
    pub branches: Vec<(Condition, Vec<Box<dyn BodyCode>>)>,
    pub last_is_guaranteed: bool,
}

impl BodyCode for BranchInstruction {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_op(&self, _symbol_table: &SymbolTable) -> Vec<Operation> {
        panic!("Invalid operation")
    }

    fn get_location(&self, _symbol_table: &SymbolTable) -> CodeLocation {
        panic!("Invalid operation")
    }

    fn get_node(&self) -> &Node {
        &self.node
    }
    
    fn to_assembly(&self, assembly: &mut Vec<Assembly>, compiler: &mut Compiler) {
        if self.last_is_guaranteed {
            todo!("Else statements")
        }
        let end_label = compiler.next_label();
        for (branch_condition, branch_code) in &self.branches {
            let next_unequal = compiler.next_label();
            assembly.push(
                Assembly::Instruction(
                    AsmInstruction::BRH { condition: branch_condition.invert(), label: next_unequal }
                )
            );

            for code in branch_code {
                code.as_ref().to_assembly(assembly, compiler);
            }

            assembly.push(
                Assembly::Instruction(
                    AsmInstruction::JMP { label: end_label }
                )
            );

            assembly.push(
                Assembly::Label(next_unequal)
            );
        }
        assembly.push(
            Assembly::Label(end_label)
        );

    }
}

pub struct LoopInstruction {
    pub node: Node
}


impl BodyCode for LoopInstruction {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_op(&self, _symbol_table: &SymbolTable) -> Vec<Operation> {
        panic!("Invalid operation")
    }

    fn get_location(&self, _symbol_table: &SymbolTable) -> CodeLocation {
        panic!("Invalid operation")
    }

    fn get_node(&self) -> &Node {
        &self.node
    }
    
    fn to_assembly(&self, assembly: &mut Vec<Assembly>, compiler: &mut Compiler) {
        todo!()
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