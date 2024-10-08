use std::path::PathBuf;

use enum_as_inner::EnumAsInner;

use super::code_location::CodeLocation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    code_location: CodeLocation,
    token_type: TokenType
}
impl Token {

    pub fn expect_constant(&self) -> u8 {
        return self.token_type.as_const_value().unwrap().bits_as_u8();
    }

    pub fn expect_register(&self) -> u8 {
        return *self.token_type.as_register().unwrap();
    }

    pub fn unwrap_identifier(self) -> String {
        if let TokenType::Identifier(i) = self.token_type {
            return i;
        }
        panic!("Expected indentifier");
    }

    pub fn copy_identifier(&self) -> String {
        return self.token_type.as_identifier().unwrap().to_owned();
    }

    pub fn to(&self, end: &Self) -> CodeLocation {
        let start = &self.code_location;
        let end = &end.code_location;
        start.to(end)
    }

    pub fn new(token_type: TokenType, code_location: CodeLocation) -> Self {
        Self {
            token_type,
            code_location
        }
    }

    pub fn eof(path: &PathBuf) -> Self {
        Self {
            token_type: TokenType::EOF,
            code_location: CodeLocation::new(path.to_owned())
        }
    }
    
    pub fn code_location(&self) -> &CodeLocation {
        &self.code_location
    }
    
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

#[derive(Debug, Clone, EnumAsInner, PartialEq, Eq)]
pub enum TokenType {
    Instr(Instr),
    Identifier(String),
    Define{read_until_semicolon: bool},
    ConstValue(ConstValue),
    Dot,
    Colon,
    Comma,
    Semicolon,
    OpenParen,
    ClosedParen,
    OpenCurly,
    ClosedCurly,
    Func,
    If,
    Else,
    Loop,
    Break,
    Continue,
    Register(u8),
    Condition(Condition),
    ParamModifier(ParamModifier),
    EOF,
}



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamModifier {
    Mut,
    In,
    Out,
    Use,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstValue {
    U8(u8),
    I8(i8),
}

impl ConstValue {
    pub fn bits_as_u8(&self) -> u8 {
        match self {
            Self::U8(v) => *v,
            Self::I8(v) => *v as u8,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    EQ,
    NE,
    GE,
    LT
}

impl Condition {
    pub fn invert(&self) -> Self {
        match self {
            Self::EQ => Self::NE,
            Self::NE => Self::EQ,
            Self::GE => Self::LT,
            Self::LT => Self::GE
        }
    }
}

impl Condition {
    ///Returns a number from 0b00 to 0b11
    pub fn to_bits(&self) -> u8 {
        match self {
            Self::EQ => 0b00,
            Self::NE => 0b01,
            Self::GE => 0b10,
            Self::LT => 0b11,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    NOP,
    HLT,
    ADD,
    SUB,
    NOR,
    AND,
    XOR,
    RSH,
    LDI,
    ADI,
    JMP,
    BRH,
    CAL,
    RET,
    LOD,
    STR
}