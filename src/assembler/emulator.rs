//Is supposed to partially "emulate" the cpu runtime, to try and do optimisations

use super::syntax::{InstructionSyntax, Node};
#[derive(Clone, Copy)]
enum RegisterEval {
    Constant(u8),
    Unknown,
}

struct Routine<'a> {
    label: &'a Node,
    child_code: Vec<&'a InstructionSyntax>
}

pub fn eval_register_optimisation(syntax: &mut Vec<Node>) {
    let mut registers = [RegisterEval::Constant(0); 16];
    let mut i = 0;
    loop { 
       // let current_instruction = syntax[i];

        
    }
}
