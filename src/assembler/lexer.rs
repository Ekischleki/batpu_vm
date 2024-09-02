
use std::{path::PathBuf, str::FromStr};

use num_bigint::{BigInt, Sign};
use phf::{phf_map, phf_set};

use super::{code_location::CodeLocation, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, file_reader::{FileReader, FileReaderError}, token::{Condition, ConstValue, Instr, ParamModifier, Token, TokenType}, type_stream::TypeStream};


static ESCAPE_MAPPING: phf::Map<&'static str, &'static str> = phf_map! {
    "n" => "\n",
    "t" => "\t",
    "\\" => "\\",
};

static IGNORE_CHARS: phf::Set<char> = phf_set! {
    ' ',
    '\t',
    '\n',
    '\r'
};

static KEYWORD_MAPPING: phf::Map<&'static str, &'static TokenType> = phf_map! {
    "define" => &TokenType::Define{read_until_semicolon: false},
    "define_s" => &TokenType::Define{read_until_semicolon: true},

    "func" => &TokenType::Func,
    "mut" => &TokenType::ParamModifier(ParamModifier::Mut),
    "in" => &TokenType::ParamModifier(ParamModifier::In),
    "out" => &TokenType::ParamModifier(ParamModifier::Out),
    "use" => &TokenType::ParamModifier(ParamModifier::Use),


    "nop" => &TokenType::Instr(Instr::NOP),
    "hlt" => &TokenType::Instr(Instr::HLT),
    "add" => &TokenType::Instr(Instr::ADD),
    "sub" => &TokenType::Instr(Instr::SUB),
    "nor" => &TokenType::Instr(Instr::NOR),
    "and" => &TokenType::Instr(Instr::AND),
    "xor" => &TokenType::Instr(Instr::XOR),
    "rsh" => &TokenType::Instr(Instr::RSH),
    "ldi" => &TokenType::Instr(Instr::LDI),
    "adi" => &TokenType::Instr(Instr::ADI),
    "jmp" => &TokenType::Instr(Instr::JMP),
    "brh" => &TokenType::Instr(Instr::BRH),
    "cal" => &TokenType::Instr(Instr::CAL),
    "ret" => &TokenType::Instr(Instr::RET),
    "lod" => &TokenType::Instr(Instr::LOD),
    "str" => &TokenType::Instr(Instr::STR),

    "eq" => &TokenType::Condition(Condition::EQ),
    "ne" => &TokenType::Condition(Condition::NE),
    "ge" => &TokenType::Condition(Condition::GE),
    "lt" => &TokenType::Condition(Condition::LT),

    "z" => &TokenType::Condition(Condition::EQ),
    "nz" => &TokenType::Condition(Condition::NE),
    "c" => &TokenType::Condition(Condition::GE),
    "nc" => &TokenType::Condition(Condition::LT),

    "r0" => &TokenType::Register(0),
    "null" => &TokenType::Register(0),

    "r1" => &TokenType::Register(1),
    "r2" => &TokenType::Register(2),
    "r3" => &TokenType::Register(3),
    "r4" => &TokenType::Register(4),
    "r5" => &TokenType::Register(5),
    "r6" => &TokenType::Register(6),
    "r7" => &TokenType::Register(7),
    "r8" => &TokenType::Register(8),
    "r9" => &TokenType::Register(9),

    "r10" => &TokenType::Register(10),
    "r11" => &TokenType::Register(11),
    "r12" => &TokenType::Register(12),
    "r13" => &TokenType::Register(13),
    "r14" => &TokenType::Register(14),
    "r15" => &TokenType::Register(15),

    "ra" => &TokenType::Register(10),
    "rb" => &TokenType::Register(11),
    "rc" => &TokenType::Register(12),
    "rd" => &TokenType::Register(13),
    "re" => &TokenType::Register(14),
    "rf" => &TokenType::Register(15),
};

static DELIM_MAPPING: phf::Map<&'static str, &'static TokenType> = phf_map! {
    "." => &TokenType::Dot,
    "," => &TokenType::Comma,

    ":" => &TokenType::Colon,
    ";" => &TokenType::Semicolon,

    "(" => &TokenType::OpenParen,
    ")" => &TokenType::ClosedParen,
    "{" => &TokenType::OpenCurly,
    "}" => &TokenType::ClosedCurly,


    "=" => &TokenType::Condition(Condition::EQ),
    "!=" => &TokenType::Condition(Condition::NE),
    "<=" => &TokenType::Condition(Condition::GE),
    ">" => &TokenType::Condition(Condition::LT),

    "_" => &TokenType::Register(0),


};


    
/// If the file reader error was a EOF error, adds a EOF token to the token stream, and returns Some(token_stream)
/// If the file reader error was unusual, it adds a diagnostic and returns None, as the tokens don't matter if incomplete.
fn on_file_reader_error(e: FileReaderError, compilation: &mut Compilation, mut token_stream: Vec<Token>, current_file: &PathBuf) -> Option<TypeStream<Token>> {
    match e {
        FileReaderError::ReachedEOF => {
            token_stream.push(Token::eof(current_file));

            Some(TypeStream::new(token_stream))
        }
        
        FileReaderError::DiagnosticError(d) => {
            compilation.add_diagnostic(d);
            None
        }
    }
}

pub fn tokenise(file_reader: &mut dyn FileReader, current_file: &PathBuf, compilation: &mut Compilation) -> Option<TypeStream<Token>> {
    let mut token_stream = vec![];
    loop {
        let current_char;

        match file_reader.peek_char() {
            Ok(c) => {
                current_char = c;
            }
            Err(e) => {
                return on_file_reader_error(e, compilation, token_stream, current_file);
            }
        }
        
        
        match current_char {
            '"' => {
                match read_string(file_reader, current_file) {
                    Ok(s) => {
                        token_stream.push(s);
                    }
                    Err(e) => {
                        compilation.add_diagnostic(e);
                    }
                }
            }
            '/' => {
                loop {
                    let comment_char;
                    match file_reader.read_char() {
                        Ok(c) => {
                            comment_char = c;
                        }
                        Err(e) => {
                            return on_file_reader_error(e, compilation, token_stream, current_file);
                        }
                    }
                    if comment_char == '\n' || comment_char == '\r' {
                        break;
                    }
                }
            }
            _ => {

                if IGNORE_CHARS.contains(&current_char) {
                    _ = file_reader.read_char(); //Jump to next char
                    continue;
                }
                match read_text(file_reader, current_file) {
                    Ok(t) => {
                        token_stream.push(t);
                    }
                    Err(e) => {
                        compilation.add_diagnostic(e);
                    }
                };

            }
        }
    };
}


fn read_text(file_reader: &mut dyn FileReader, current_file: &PathBuf) -> Result<Token, Diagnostic> {
    let first_char = file_reader.peek_char().expect("Unexpected file reader error");
    if first_char.is_ascii_digit() || first_char == '-' {
        read_number(file_reader, current_file)
    } else if first_char.is_alphanumeric() {
        read_keyword(file_reader, current_file)
    } else {
        read_delim(file_reader, current_file)
    }
}

fn read_number(file_reader: &mut dyn FileReader, current_file: &PathBuf) -> Result<Token, Diagnostic> {
    let start_char = file_reader.get_position();
    let mut number = String::new();
    let mut number_char = file_reader.read_char().expect("Unintended file reading error.");
    while number_char.is_ascii_digit() || number_char == '.' {
        if IGNORE_CHARS.contains(&number_char) {
            break;
        }
        number.push(number_char);

        match file_reader.read_char() {
            Ok(c) => {
                number_char = c;
            }
            Err(_) => {
                break; //The current char implementation will take care of the error later :3. Def sounds like a ticking time-bomb, but not for me to worry about
            }
        }                    
    }
    file_reader.set_position(file_reader.get_position() - 1); //This should in theory not cause an underflow exception

    let token_type = find_smallest_type(number, 
        CodeLocation::with_section(
            current_file.to_owned(),
            start_char,
            file_reader.get_position()))?;

    return Ok(Token::new(
        token_type,
        CodeLocation::with_section(
            current_file.to_owned(),
            start_char,
            file_reader.get_position())
    ));
}

fn bit_length_with_sign(big_int: &BigInt) -> u64 {
    let bit_length = big_int.bits();
    match big_int.sign() {
        Sign::Minus => {
            bit_length + 1
        },
        Sign::Plus => {
            bit_length
        },
        Sign::NoSign => {1}
    }
}

fn find_smallest_type(number: String, code_location: CodeLocation) -> Result<TokenType, Diagnostic> {

    if number.contains('.') {
        todo!("Floating point numbers")
    }

    let integer;

    match BigInt::from_str(&number) {
        Ok(bi) => {
            integer = bi;
        }
        Err(_) => {
            return Err(Diagnostic::new(
                DiagnosticType::Error,
                "Unable to parse integer".to_owned(),
                Some(code_location),
                DiagnosticPipelineLocation::Lexing
            ));
        }
    }

    let bit_length = bit_length_with_sign(&integer);
    let is_positive = integer.ge(&BigInt::from(0));


    match bit_length {
        0..=8 => {
            if is_positive {
                return Ok(TokenType::ConstValue(ConstValue::U8(integer.try_into().unwrap())));
            } else {
                return Ok(TokenType::ConstValue(ConstValue::I8(integer.try_into().unwrap())));
            }
        },
        /*
        9..=16 => {
            if is_positive {
                return Ok(TokenType::U16(integer.try_into().unwrap()));
            } else {
                return Ok(TokenType::I16(integer.try_into().unwrap()));
            }
        },
        17..=32 => {
            if is_positive {
                return Ok(TokenType::U32(integer.try_into().unwrap()));
            } else {
                return Ok(TokenType::I32(integer.try_into().unwrap()));
            }
        }
        33..=64 => {
            if is_positive {
                return Ok(TokenType::U64(integer.try_into().unwrap()));
            } else {
                return Ok(TokenType::I64(integer.try_into().unwrap()));
            }
        }
        _ => {
            return Ok(TokenType::BigInt(integer));
        }
            */
        _ => {
            return Err(Diagnostic::new(
                DiagnosticType::Error,
                "Integer is too big for current system.".to_owned(),
                Some(code_location),
                DiagnosticPipelineLocation::Lexing
            ));
        }
    }
}

fn read_delim(file_reader: &mut dyn FileReader, current_file: &PathBuf) -> Result<Token, Diagnostic> {
    let start_char = file_reader.get_position();
    let mut delim = String::with_capacity(2);
    loop {
        let delim_char;
        match file_reader.read_char() {
            Ok(c) => {
                delim_char = c;
            }
            Err(_) => {
                break; //The current char implementation will take care of the error later :3. Def sounds like a ticking time-bomb, but not for me to worry about
            }
        }  
        delim.push(delim_char);
        if !DELIM_MAPPING.contains_key(&delim) {
            delim.pop();
            break;
        }
    }
    if delim.len() != 0 {
        file_reader.set_position(file_reader.get_position() - 1);
    }
    let code_location = CodeLocation::with_section(current_file.to_owned(),
    start_char,
    file_reader.get_position()
    );
    let delim_type = DELIM_MAPPING.get(&delim);

    match delim_type {
        Some(token_type) => {
            return Ok(Token::new(
                (**token_type).clone(),
                code_location
                ));
        }
        None => {
            return Err(Diagnostic::new(
                DiagnosticType::Error,
                format!("Invalid delimitor"),
                Some(code_location),
                DiagnosticPipelineLocation::Lexing
            ));
        }
    }
}

fn read_keyword(file_reader: &mut dyn FileReader, current_file: &PathBuf) -> Result<Token, Diagnostic> {
    let start_char = file_reader.get_position();
    let mut keyword = String::new();
    let mut keyword_char = file_reader.read_char().expect("Unintended file reading error.");
    while keyword_char.is_alphanumeric() || keyword_char == '_' {
        if IGNORE_CHARS.contains(&keyword_char) {
            break;
        }
        keyword.push(keyword_char);

        match file_reader.read_char() {
            Ok(c) => {
                keyword_char = c;
            }
            Err(_) => {
                break; //The current char implementation will take care of the error later :3. Def sounds like a ticking time-bomb, but not for me to worry about
            }
        }                    
    }
    file_reader.set_position(file_reader.get_position() - 1); //This should in theory not cause an underflow exception

    let keyword_type = KEYWORD_MAPPING.get(&keyword);
    let code_location = CodeLocation::with_section(current_file.to_owned(),
    start_char,
    file_reader.get_position()
    );
    match keyword_type {
        Some(token_type) => {
            return Ok(Token::new(
                (**token_type).clone(),
                code_location
                ));
        }
        None => {
            return Ok(Token::new(
            TokenType::Identifier(keyword),
            code_location
            ))
        }
    }
}

fn read_string(file_reader: &mut dyn FileReader, current_file: &PathBuf) -> Result<Token, Diagnostic> {

    let location_begin = file_reader.get_position();
        file_reader.set_position(file_reader.get_position() + 1); //We don't care about the initial '"'.

    let mut string = String::new();
    let mut read: char;
    loop {
        match file_reader.read_char() {
            Ok(c) => {read = c;}
            Err(e) => {
                match e {
                    FileReaderError::ReachedEOF => {
                        return Err(Diagnostic::new(
                            DiagnosticType::Error,
                            "Expected '\" to mark end of string'.".to_owned(),
                            Some(
                                CodeLocation::with_section(current_file.to_owned(), location_begin, file_reader.get_position())
                            ),
                            DiagnosticPipelineLocation::Lexing
                        ));
                    }
                    FileReaderError::DiagnosticError(d) => {
                        return Err(d);
                    }
                }
            }
        }
        if read == '"' {
            break;
        }
        if read == '\\' { //Escape, todo: currently no complex value insertations
            let escape_char ;
            match file_reader.read_char() {
                Ok(c) => {escape_char = c;}
                Err(e) => {
                    match e {
                        FileReaderError::ReachedEOF => {
                            return Err(Diagnostic::new(
                                DiagnosticType::Error,
                                "Expected followup character for escape, and end of string.".to_owned(),
                                Some(
                                    CodeLocation::with_section(current_file.to_owned(), location_begin, file_reader.get_position())
                                ),
                                DiagnosticPipelineLocation::Lexing
                            ));
                        }
                        FileReaderError::DiagnosticError(d) => {
                            return Err(d);
                        }
                    }
                }
            }
            let replace_char = ESCAPE_MAPPING[escape_char.to_string().as_str()];
            string.push_str(replace_char);
        }
        else {
            string.push(read);
        }
    }

    return Ok(
        Token::new(
            TokenType::ConstValue(ConstValue::String(string)),
            CodeLocation::with_section(
                current_file.to_owned(), 
                location_begin, 
                file_reader.get_position())));
}
