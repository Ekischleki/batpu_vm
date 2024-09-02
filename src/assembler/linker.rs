use std::collections::HashMap;

use crate::assembler::diagnostic::{DiagnosticPipelineLocation, DiagnosticType};

use super::{diagnostic::Diagnostic, source_mapping::SourceMappings, symbol_table::SymbolTable, syntax::{InstructionSyntax, Node}, token::{Token, TokenType}};

pub struct AssemblyBuilder {
    labels: HashMap<String, usize>, //Identifier + pointing instruction
    label_resolve: Vec<(String, usize)>, //Identifier + pointing instruction
}

impl AssemblyBuilder {


    pub fn new() -> Self {
        Self { labels: HashMap::new(), label_resolve: vec![] }
    }

    pub fn build_asm(mut self, ast: Vec<Node>) -> Result<([u8; 2048], SourceMappings), Diagnostic> {
        let mut source_mappings = SourceMappings::new();

        let mut assembly_builder = vec![];
        //Generate assembly without inserting labels.
        for syntax in ast {
            match syntax {
                Node::Instruction { original_instruction, instruction_syntax } => {
                    let asm_start = assembly_builder.len();
                    assembly_builder.extend_from_slice(&self.build_instruction(assembly_builder.len(), instruction_syntax));
                    let asm_end = assembly_builder.len();

                    source_mappings.push_mapping(original_instruction.code_location(), asm_start, asm_end);
                }
                Node::Label { dot: _, identifier } => {
                    self.labels.insert(identifier.unwrap_identifier(), assembly_builder.len());
                }
            }
        }
        if assembly_builder.len() > 2048 {
            return Err(Diagnostic::new(
                DiagnosticType::Error, 
                format!("The resulting assembly is with {} bytes too large. The allowed maximum is 2048 bytes, that means you're {} bytes over the limit.", assembly_builder.len(), assembly_builder.len() - 2048), 
                None, 
                DiagnosticPipelineLocation::Assembling));
        }
        //Insert labels, now that size is known
        for (label_name, insert_location) in self.label_resolve {
            let label_location = self.labels.get(&label_name).unwrap();
            Self::insert_address(&mut assembly_builder, insert_location, *label_location);
        }

        let mut assembly = [1; 2048]; //Unused space is the HLT instruction, as a failsafe 
        for (i, &val) in assembly_builder.iter().enumerate() {
            assembly[i] = val;
        }

        return Ok((assembly, source_mappings));
    }

    fn insert_address(assembly: &mut Vec<u8>, insert_location: usize, insert_adr: usize) {
        assert!(insert_adr <= 2048); //Max address cpu can handle
        assert!(insert_adr & 1 == 0); //Cpu can only jump to even instructions (Instructions are fixed-size, so they should always)
        let insert_adr: u16 = insert_adr as u16 >> 1;
        let bitmask_left = (insert_adr >> 8) as u8;
        let bitmask_right = (insert_adr & 0b1111_1111) as u8;
        assembly[insert_location] |= bitmask_left;
        assembly[insert_location + 1] |= bitmask_right;

    }

    fn build_instruction(&mut self, asm_location: usize, intr: InstructionSyntax) -> [u8; 2] {
        match intr {
            InstructionSyntax::NOP => {
                const INSTR_OPCODE: u8 = place_byte(0, 0);
                [INSTR_OPCODE, 0]
            }
            InstructionSyntax::HLT => {
                const INSTR_OPCODE: u8 = place_byte(1, 0);
                [INSTR_OPCODE, 0]
            }
            InstructionSyntax::ADD { a, b, dest } => {
                [
                    place_byte(2, get_reg_indx(a)),
                    place_byte(get_reg_indx(b), get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::SUB { a, b, dest } => {
                [
                    place_byte(3, get_reg_indx(a)),
                    place_byte(get_reg_indx(b), get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::NOR { a, b, dest } => {
                [
                    place_byte(4, get_reg_indx(a)),
                    place_byte(get_reg_indx(b), get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::AND { a, b, dest } => {
                [
                    place_byte(5, get_reg_indx(a)),
                    place_byte(get_reg_indx(b), get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::XOR { a, b, dest } => {
                [
                    place_byte(6, get_reg_indx(a)),
                    place_byte(get_reg_indx(b), get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::RSH { a, dest } => {
                [
                    place_byte(7, get_reg_indx(a)),
                    place_byte(0, get_reg_indx(dest)),
                ]
            }
            InstructionSyntax::LDI { a, immediate } => {
                [
                    place_byte(8, get_reg_indx(a)),
                    get_immediate_value(immediate),
                ]
            }
            InstructionSyntax::ADI { a, immediate } => {
                [
                    place_byte(9, get_reg_indx(a)),
                    get_immediate_value(immediate),
                ]
            }
            InstructionSyntax::JMP { label } => {
                self.register_label_resolve_address(asm_location, *label);
                [
                    place_byte(10, 0), //The address will be inserted later, as soon as the locations of all the labels are known
                    0
                ]
            }
            InstructionSyntax::BRH { condition, label } => {
                let condition_bits;
                if let TokenType::Condition(c) = condition.token_type() {
                    condition_bits = c.to_bits() << 2;
                } else {
                    panic!("Expected condition")
                }
                self.register_label_resolve_address(asm_location, *label);
                [
                    place_byte(11, condition_bits), //Adr, resolved later
                    0
                ]

            }
            InstructionSyntax::CAL { label } => {
                self.register_label_resolve_address(asm_location, *label);
                [
                    place_byte(12, 0), //Adr, resolved later
                    0
                ]
            }
            InstructionSyntax::RET => {
                [
                    place_byte(13, 0), 
                    0
                ]
            }

            InstructionSyntax::LOD { a, dest, offset } => {
                [
                    place_byte(14, get_reg_indx(a)),
                    place_byte(get_reg_indx(dest), get_offset(offset)),
                ]
            }

            InstructionSyntax::STR { a, source, offset } => {
                [
                    place_byte(15, get_reg_indx(a)),
                    place_byte(get_reg_indx(source), get_offset(offset)),
                ]
            }
        }
    }

   

    fn register_label_resolve_address(&mut self, asm_location: usize, label: Node) {
        let label_name;
        if let Node::Label { dot: _, identifier } = label {
            if let TokenType::Identifier(l) = identifier.token_type() {
                label_name = l.clone();
            }
            else {
                panic!("Expected identifier")
            }
        }
        else {
            panic!("Expected label")
        }
        self.label_resolve.push((label_name, asm_location))
    }


}


fn get_offset(offset: Token) -> u8 {
    if let TokenType::ConstValue(cs_val) = offset.token_type() {
        let offset: i8 = match cs_val {
            crate::assembler::token::ConstValue::U8(u8) => *u8 as i8,
            crate::assembler::token::ConstValue::I8(i8) => *i8,
            crate::assembler::token::ConstValue::String(_) => panic!("Unexpected string"),
        };

        if offset < 0 {
            return (offset | 0b0000_1000 & 0b0000_1111) as u8; //should do the trick, right?
        } else {
            return (offset & 0b0000_0111) as u8;
        }
    }
    panic!("Expected const value");
}
fn get_immediate_value(token: Token) -> u8 {
    if let TokenType::ConstValue(cs_val) = token.token_type() {
        return match cs_val {
            crate::assembler::token::ConstValue::U8(u8) => *u8,
            crate::assembler::token::ConstValue::I8(i8) => *i8 as u8,
            crate::assembler::token::ConstValue::String(_) => todo!(),
        };
    }
    panic!("Expected const value");

}

fn get_reg_indx(reg_token: Token) -> u8 {
    if let TokenType::Register(idx) = reg_token.token_type() {
        return *idx;
    }
    panic!("Expected register token");
}

const fn place_byte(left_bits: u8, right_bits: u8) -> u8 {
    assert!(left_bits < 16);
    assert!(right_bits < 16);

    return (left_bits << 4) | right_bits
}