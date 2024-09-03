use std::collections::HashMap;

use super::{code_location::CodeLocation, compilation::Compilation, diagnostic::{Diagnostic, DiagnosticPipelineLocation, DiagnosticType}, syntax::{Arg, InstructionSyntax, Node}, token::{self, Condition, ConstValue, Instr, Token, TokenType}, type_stream::TypeStream};

pub fn peek_token_type(token_stream: &TypeStream<Token>) -> TokenType {
    token_stream.extract(|t| t.token_type().clone())
}

pub fn token_or_diagnostic(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>, expected_token: TokenType) -> Option<Token> {
    if !expect_token(compilation, token_stream, expected_token) {
        None
    } else {
        Some(token_stream.next())
    }
}

pub fn replace_defines(compilation: &mut Compilation, mut token_stream: TypeStream<Token>) -> TypeStream<Token> {
    let mut defined_tokens: HashMap<String, Vec<Token>> = HashMap::new();
    let mut result_stream = vec![];
    loop {
        let token_type = peek_token_type(&token_stream);

        match token_type {
            TokenType::Define { read_until_semicolon } => {
                let define_keyword = token_stream.next();
                if !expect_token(compilation, &token_stream, TokenType::Identifier(String::new())) {
                    continue;
                }
                let name;
                if let TokenType::Identifier(n) = token_stream.next().token_type() {
                    name = n.clone();
                } else {panic!("Expected identifier")}

                let define_tokens = read_define(&mut token_stream, compilation, read_until_semicolon, &define_keyword);
                if let Some(define_tokens) = define_tokens {
                    defined_tokens.insert(name, define_tokens);
                }
            }
            TokenType::Dot => {
                result_stream.push(token_stream.next());
                let next_token = token_stream.next();
                if let TokenType::Identifier(n) = next_token.token_type() {
                    if defined_tokens.contains_key(n) {
                        compilation.add_diagnostic(Diagnostic::new(
                            DiagnosticType::Info, 
                            "Didn't expand this defined keyword, as it is a label.".to_owned(), 
                            Some(next_token.code_location().clone()), DiagnosticPipelineLocation::Parsing));
                    }
                }
                result_stream.push(next_token);
            }
            TokenType::Identifier(i) => {
                match defined_tokens.get(&i) {
                    Some(r) => {
                        let mut expanded = r.clone();
                        result_stream.append(&mut expanded);
                        _ = token_stream.next();

                    }
                    None => {
                        result_stream.push(token_stream.next())
                    } //We don't care about this identifier
                }
            }
            TokenType::EOF => {
                result_stream.push(token_stream.next());
                println!("{:#?}", result_stream);
                return TypeStream::new(result_stream);
            }
            _ => {
                result_stream.push(token_stream.next());
            }
        }
    }
}

pub fn read_define(token_stream: &mut TypeStream<Token>, compilation: &mut Compilation, read_until_semicolon: bool, define_keyword: &Token) -> Option<Vec<Token>> {
    let mut define_buffer = vec![];
    loop {
        let current_token = peek_token_type(token_stream);
        match current_token {
            TokenType::Semicolon => {
                if read_until_semicolon {
                    _ = token_stream.next();
                    return Some(define_buffer);
                } else {
                    define_buffer.push(token_stream.next());
                }
            }
            TokenType::Define { read_until_semicolon: _ } => {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    "Defines can't contain the define keyword. Did you forget to place a semicolon?".to_owned(), 
                    Some(define_keyword.code_location().clone()), DiagnosticPipelineLocation::Parsing));
                    return None;
            }
            TokenType::EOF => {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    if read_until_semicolon {
                        "Expected semicolon. Did you mean to use 'define' instead of 'define_s'"
                    } else {
                        "Expected token, got EOF. Tip: delete this keyword."
                    }.to_owned(), 
                    Some(define_keyword.code_location().clone()), DiagnosticPipelineLocation::Parsing));
                    return None;
            }
            _ => {
                define_buffer.push(token_stream.next());
            }
        }
        if !read_until_semicolon {
            return Some(define_buffer);
        }
    }
}

pub fn read_func(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Option<Node> {
    let func_keyword = token_stream.next(); //We know it is the keyword.
    let identifier = token_or_diagnostic(compilation, token_stream, TokenType::Identifier(String::new()))?;
    _ = token_or_diagnostic(compilation, token_stream, TokenType::OpenParen)?;
    let args = read_args(compilation, token_stream);
    _ = token_or_diagnostic(compilation, token_stream, TokenType::OpenCurly)?;

    let body = read_body(compilation, token_stream).into_iter().map(|node| Box::from(node));
    let body = TypeStream::new(body.collect());
    
    Some(Node::Func { func_keyword, identifier, args,  body })
}

pub fn read_func_call(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Option<Node> {
    let identifier = token_stream.next();
    _ = token_or_diagnostic(compilation, token_stream, TokenType::OpenParen)?;
    let args = read_args(compilation, token_stream);
    Some(Node::FuncCall { identifier, args })
}

pub fn read_body(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Vec<Node> {
    let mut output = vec![];
    loop {
        let token_type = peek_token_type(token_stream);
        match token_type {
            TokenType::If => {

            }

            TokenType::Dot => {
                match read_label(compilation, token_stream) {
                    Some(l) => {
                        output.push(l);
                    }
                    _ => {}
                }
            } 

            TokenType::Identifier(_) => {
                let func_call = read_func_call(compilation, token_stream);
                func_call.map(|f| output.push(f));
            }

            TokenType::ClosedCurly => {
                _ = token_stream.next();
                return output;
            }

            TokenType::EOF => { 
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Unexpected token EOF. Consider adding a '}}'"), 
                    Some(token_stream.next().code_location().clone()),  //This will definitely cause crashes, ill fix that later tho
                    DiagnosticPipelineLocation::Parsing));
                return output; 
            }

            TokenType::Instr(_) => {
                match read_instruction(compilation, token_stream) {
                    Some(i) => {
                        output.push(i);
                    }
                    _ => {}
                }
            }

            _ => {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Unexpected token '{:#?}'", token_type), 
                    Some(token_stream.next().code_location().clone()), 
                    DiagnosticPipelineLocation::Parsing));
            }
        }

    }
}

pub fn read_args(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Vec<Arg> {
    let mut res = vec![];
    if let TokenType::ClosedParen = peek_token_type(token_stream) {
        token_stream.next();
        return res;
    }
    loop {
        read_arg(compilation, token_stream).map(|arg| res.push(arg));
        let follow_token = peek_token_type(token_stream);

        match follow_token {
            TokenType::ClosedParen => {
                token_stream.next();
                return res;
            }
            TokenType::Comma => {token_stream.next();}
            _ => {
                compilation.add_diagnostic(
                    Diagnostic::new(
                        DiagnosticType::Error, 
                        format!("Expected ')' or comma, got {:#?}", follow_token), 
                        Some(token_stream.next().code_location().clone()), 
                        DiagnosticPipelineLocation::Parsing)
                );
                return res;
            }

        }
    }
}

pub fn read_arg(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Option<Arg> {
    let current_token = peek_token_type(token_stream);
    match current_token {
        TokenType::Register(i) =>  {Some(Arg {register: i, modifier: None, register_token: token_stream.next()})}
        TokenType::ParamModifier(_) => {
            let modifier = token_stream.next();
            let reg = token_or_diagnostic(compilation, token_stream, TokenType::Register(0))?;
            if let TokenType::Register(i) = reg.token_type() {
                Some(Arg {register: *i, modifier: Some(modifier), register_token: reg})
            } else {
                panic!("Expected register");
            }
        }
        _ => {
            
            compilation.add_diagnostic(Diagnostic::new(
                DiagnosticType::Error, 
                format!("Expected register or modifier, got: {:#?}", current_token), 
                Some(peek_location(token_stream)), 
                DiagnosticPipelineLocation::Parsing));
                None
        }
    }
}

pub fn parse(compilation: &mut Compilation, token_stream: TypeStream<Token>) -> Vec<Node> {
    let mut token_stream = replace_defines(compilation, token_stream);
    //println!("{:#?}", token_stream);
    let mut output = vec![];
    loop {
        let token_type = peek_token_type(&token_stream);
        match token_type {
            TokenType::Func => {
                read_func(compilation, &mut token_stream).map(|f| output.push(f));
            }

            TokenType::EOF => { return output; }

            _ => {
                compilation.add_diagnostic(Diagnostic::new(
                    DiagnosticType::Error, 
                    format!("Unexpected token '{:#?}'", token_type), 
                    Some(token_stream.next().code_location().clone()), 
                    DiagnosticPipelineLocation::Parsing));
            }
        }

    }
}

fn read_instruction(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<Node> {
    let instr_token = tokens.next();
    let instr;
    match instr_token.token_type() {
        TokenType::Instr(i) => {
            instr = i;
        }
        _ => {
            panic!("called read instruction without providing instruction token")
        }
    }

    match instr {
        Instr::NOP => {
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::NOP })
        }
        Instr::HLT => {
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::HLT })
        }
        Instr::ADD => {
            let (a, b, c) = read_abc(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::ADD { a, b, dest: c } })
        }
        Instr::SUB => {
            let (a, b, c) = read_abc(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::SUB { a, b, dest: c } })
        }
        Instr::NOR => {
            let (a, b, c) = read_abc(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::NOR { a, b, dest: c } })
        }
        Instr::AND => {
            let (a, b, c) = read_abc(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::AND { a, b, dest: c } })
        }
        Instr::XOR => {
            let (a, b, c) = read_abc(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::XOR { a, b, dest: c } })
        }
        Instr::RSH => {
            let (a, b) = read_ab(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::RSH { a, dest: b } })
        }

        Instr::LDI => {
            let (a, immediate) = read_immediate(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::LDI { a, immediate } })
        }
        Instr::ADI => {
            let (a, immediate) = read_immediate(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::ADI { a, immediate } })
        }

        Instr::JMP => {
            let label = Box::new(read_label(compilation, tokens)?);
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::JMP { label } })
        }
        Instr::BRH => {
            let condition = read_condition(compilation, tokens)?;
            let label = Box::new(read_label(compilation, tokens)?);
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::BRH { condition, label }  })
        }
        Instr::CAL => {
            let label = Box::new(read_label(compilation, tokens)?);
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::CAL { label } })
        }
        Instr::RET => {
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::RET })
        }
        Instr::STR => {
            let (a, source, offset) = read_ab_offset(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::STR { a, source, offset } })
        }
        Instr::LOD => {
            let (a, dest, offset) = read_ab_offset(compilation, tokens)?;
            Some(Node::Instruction { original_instruction: instr_token, instruction_syntax: InstructionSyntax::LOD { a, dest, offset } })
        }

    }
    //todo!()
}

fn read_ab_offset(compilation: &mut Compilation, token_stream: &mut TypeStream<Token>) -> Option<(Token, Token, Token)> {
    let (a, b) = read_ab(compilation, token_stream)?;
    let offset = if let TokenType::ConstValue(ConstValue::I8(_)) = peek_token_type(token_stream) {
        token_stream.next()
    } else {
        Token::new(TokenType::ConstValue(ConstValue::I8(0)), b.code_location().to_owned())
    };
    Some((a, b, offset))
}

fn read_immediate(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<(Token, Token)> {
    let a = read_register(compilation, tokens)?;
    let val = read_constant(compilation, tokens)?;
    Some((a, val))
}

fn read_abc(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<(Token, Token, Token)> {

    let a = read_register(compilation, tokens)?;
    let b = read_register(compilation, tokens)?;
    let c = read_register(compilation, tokens)?;
    Some((a, b, c))
}

fn read_ab(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<(Token, Token)> {
    let a = read_register(compilation, tokens)?;
    let b = read_register(compilation, tokens)?;
    Some((a, b))
}

fn read_register(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<Token> {
    if !expect_token(compilation, tokens, TokenType::Register(0)) {
        return None;
    }
    Some(tokens.next())
} 
fn read_constant(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<Token>  {
    if !expect_token(compilation, tokens, TokenType::ConstValue(ConstValue::I8(0))) {
        return None;
    }
    Some(tokens.next())
}
fn read_condition(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<Token>  {
    if !expect_token(compilation, tokens, TokenType::Condition(Condition::EQ)) {
        return None;
    }
    Some(tokens.next())
}

fn read_label(compilation: &mut Compilation, tokens: &mut TypeStream<Token>) -> Option<Node> {
    let dot = tokens.next();
    if !expect_token(compilation, tokens, TokenType::Identifier(String::new())) {
        return None;
    }
    let identifier = tokens.next();
    Some(
        Node::Label { dot, identifier }
    )
}

fn peek_location(token_stream: &TypeStream<Token>) -> CodeLocation {
    token_stream.extract(|t| t.code_location().clone())
}

fn expect_token(compilation: &mut Compilation, token_stream: &TypeStream<Token>, expected_token: TokenType) -> bool {

    let token_type = peek_token_type(token_stream);

    if std::mem::discriminant(&token_type) != std::mem::discriminant(&expected_token) {
        let faulty_token_location = peek_location(token_stream);

        compilation.add_diagnostic(Diagnostic::new(
            DiagnosticType::Error, 
            format!("Expected token '{:#?}', got token '{:#?}'", expected_token, token_type), 
            Some(faulty_token_location), 
            DiagnosticPipelineLocation::Parsing));
        return false;
    }
    return true;
}