use super::token::{Token, TokenType};
#[derive(Debug)]
pub enum Syntax {
    Instruction {original_instruction: Token, instruction_syntax: InstructionSyntax},
    Label {dot: Token, identifier: Token},
    Func {
        func_keyword: Token, 
        identifier: Token, 
        args: Vec<Arg>,
        body: Vec<Syntax>,
    },
    FuncCall {
        identifier: Token,
        args: Vec<Arg>,

    }

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
pub struct Arg {
    pub register: u8,
    pub modifier: Option<Token>,
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

    pub fn get_op(&self) -> Vec<Operation> { //Keeping registers safe since 1954
        match self {
            Self::NOP => vec![],
            Self::HLT => vec![],
            Self::ADD { a, b, dest } => 
            vec![
                Operation::ReadReg(a.expect_register()), 
                Operation::ReadReg(b.expect_register()), 
                Operation::WriteReg(dest.expect_register())
                ],
            Self::SUB { a, b, dest } => 
            vec![
                Operation::ReadReg(a.expect_register()), 
                Operation::ReadReg(b.expect_register()), 
                Operation::WriteReg(dest.expect_register())
                ],
            Self::NOR { a, b, dest } => 
            vec![
                Operation::ReadReg(a.expect_register()), 
                Operation::ReadReg(b.expect_register()), 
                Operation::WriteReg(dest.expect_register())
                ],
            Self::AND { a, b, dest } => 
                vec![
                    Operation::ReadReg(a.expect_register()), 
                    Operation::ReadReg(b.expect_register()), 
                    Operation::WriteReg(dest.expect_register())
                    
                    ],
            Self::XOR { a, b, dest } => 
            vec![
                Operation::ReadReg(a.expect_register()), 
                Operation::ReadReg(b.expect_register()), 
                Operation::WriteReg(dest.expect_register())
                ],
            
            Self::RSH { a, dest } => 
            vec![
                Operation::ReadReg(a.expect_register()), 
                Operation::WriteReg(dest.expect_register())
                ],

            Self::LDI { a, immediate: _ } =>             
            vec![
                Operation::WriteReg(a.expect_register())
                ],
            
            Self::ADI { a, immediate: _ } =>             
            vec![
                Operation::ReadReg(a.expect_register()),
                Operation::WriteReg(a.expect_register())
                ],
            Self::JMP { label: _ } => vec![],
            Self::BRH { label: _ , condition: _} => vec![],
            Self::CAL { label: _ } => vec![],
            Self::RET => vec![],
            Self::LOD { a, dest, offset: _ } => 
            vec![
                Operation::ReadReg(a.expect_register()),
                Operation::WriteReg(dest.expect_register())
            ],
            Self::STR { a, source, offset: _ } => 
            vec![
                Operation::ReadReg(a.expect_register()),
                Operation::ReadReg(source.expect_register())
            ]
        }

    }

}


pub enum Operation {
    ReadReg(u8),
    WriteReg(u8),
}