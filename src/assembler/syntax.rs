use enum_as_inner::EnumAsInner;

use super::{code_location::CodeLocation, token::{Token, TokenType}, type_stream::TypeStream};
#[derive(Debug, EnumAsInner)]
pub enum Node {
    Instruction {original_instruction: Token, instruction_syntax: InstructionSyntax},
    If {keyword: Token, body: TypeStream<Box<Node>>},
    IfElse {if_keyword: Token, true_body: TypeStream<Box<Node>>, else_keyword: Token, false_body: TypeStream<Box<Node>>},
    Loop {keyword: Token, body: TypeStream<Box<Node>>},

    Label {dot: Token, identifier: Token},
    Func {
        func_keyword: Token, 
        identifier: Token, 
        args: Vec<Arg>,
        body: TypeStream<Box<Node>>,
    },
    FuncCall {
        identifier: Token,
        args: Vec<Arg>,

    }

}

impl Node {

    pub fn get_blame_location(&self) -> CodeLocation {
        match self {
            Node::If { keyword,.. } => {
                keyword.code_location().to_owned()
            }
            Node::IfElse { if_keyword,.. } => {
                if_keyword.code_location().to_owned()
            }
            Node::Loop { keyword,.. } => {
                keyword.code_location().to_owned()
            }
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
    pub register_token: Token,
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


