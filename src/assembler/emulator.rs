//Is supposed to partially "emulate" the cpu runtime, to try and do optimisations

use super::syntax::{InstructionSyntax, Syntax};
#[derive(Clone, Copy)]
enum RegisterEval {
    Constant(u8),
    Unknown,
}

struct Routine<'a> {
    label: &'a Syntax,
    child_code: Vec<&'a InstructionSyntax>
}

pub fn eval_register_optimisation(syntax: &mut Vec<Syntax>) {
    let mut registers = [RegisterEval::Constant(0); 16];
    let mut i = 0;
    loop { 
       // let current_instruction = syntax[i];

        
    }
}
