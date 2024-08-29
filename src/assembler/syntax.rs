use enum_as_inner::EnumAsInner;

use super::{code_location::CodeLocation, symbol_table::{BodyCode, Operation}, token::{Token, TokenType}};
#[derive(Debug, EnumAsInner)]
pub enum Node {
    Instruction {original_instruction: Token, instruction_syntax: InstructionSyntax},
    Label {dot: Token, identifier: Token},
    Func {
        func_keyword: Token, 
        identifier: Token, 
        args: Vec<Arg>,
        body: Vec<Node>,
    },
    FuncCall {
        identifier: Token,
        args: Vec<Arg>,

    }

}

impl Node {

    pub fn get_blame_location(&self) -> CodeLocation {
        match self {
            Node::Func { func_keyword, .. } => {
                func_keyword.code_location().to_owned()
            }
            Node::FuncCall { identifier,.. } => {
                identifier.code_location().to_owned()
            }
            Node::Instruction { original_instruction,.. } => {
                original_instruction.code_location().to_owned()
            }
            Node::Label { dot, .. } => {
                dot.code_location().to_owned()
            }
        }
    }

    pub fn get_identifier(&self) -> Option<&String> {
        let identifier_token = match self {
            Node::Label { dot: _, identifier } => Some(identifier),

            _ => None
        }?;

        if let TokenType::Identifier(s) = identifier_token.token_type() {
            return Some(s);
        } else { 
            panic!("Expected identifier");
        }
    }

    
}


#[derive(Debug)]
pub struct Arg {
    pub register: u8,
    pub modifier: Option<Token>,
}

#[derive(Debug, EnumAsInner)]
pub enum InstructionSyntax {
    NOP,
    HLT,
    ADD{a: Token, b: Token, dest: Token},
    SUB{a: Token, b: Token, dest: Token},
    NOR{a: Token, b: Token, dest: Token},
    AND{a: Token, b: Token, dest: Token},
    XOR{a: Token, b: Token, dest: Token},
    RSH{a: Token, dest: Token},
    LDI{a: Token, immediate: Token},
    ADI{a: Token, immediate: Token},
    JMP{label: Box<Node>},
    BRH{condition: Token, label: Box<Node>},
    CAL{label: Box<Node>},
    RET,
    LOD{a: Token, dest: Token, offset: Token},
    STR{a: Token, source: Token, offset: Token}
}



impl InstructionSyntax {
    pub fn get_label(&self) -> Option<&String> {
        match self {
            Self::JMP { label } => label.as_ref().get_identifier(),
            Self::BRH { condition: _, label } => label.as_ref().get_identifier(),
            Self::CAL { label } => label.as_ref().get_identifier(),
            _ => {None}
        }
    }

    

}


