use super::token::{Token, TokenType};
#[derive(Debug)]
pub enum Syntax {
    Instruction {original_instruction: Token, instruction_syntax: InstructionSyntax},
    Label {dot: Token, identifier: Token},
}

impl Syntax {
    pub fn get_identifier(&self) -> Option<&String> {
        let identifier_token = match self {
            Syntax::Label { dot: _, identifier } => Some(identifier),

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
    JMP{label: Box<Syntax>},
    BRH{condition: Token, label: Box<Syntax>},
    CAL{label: Box<Syntax>},
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