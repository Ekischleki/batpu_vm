use super::compiler::{AsmInstruction, Assembly};

pub fn optimize(assembly: Vec<Assembly>) -> Vec<Assembly> {
    let assembly = remove_double_jumps(assembly);
    let assembly = remove_next_jumps(assembly);
    assembly
}


fn remove_next_jumps(assembly: Vec<Assembly>) -> Vec<Assembly> {
    let mut res = vec![];

    let stepper = AssemblyStepper::new(assembly);
    let mut i = 0;

    loop {
        let is_valid = stepper.is_valid(i);

        if !is_valid {
            return res;
        }


        let instr = stepper.idx(i);



        if let Assembly::Instruction(AsmInstruction::JMP { label }) = instr {
            let next_instr = stepper.jmp_next_instruction(i + 1);
            let jump_dest = stepper.jmp_label(*label);
            if let Some(next_instr) = next_instr {
                if next_instr == jump_dest {
                    i += 1;

                    continue;
                }
            }
        }

        res.push(instr.clone());
        i += 1;


    }
}

///If we jump to a label that immediateley jumps to another label, we can save one jump and jump to that label directly
fn remove_double_jumps(assembly: Vec<Assembly>) -> Vec<Assembly> {
    let mut res = vec![];

    let stepper = AssemblyStepper::new(assembly);
    let mut i = 0;
    loop {
        let is_valid = stepper.is_valid(i);

        if !is_valid {
            return res;
        }

        let instr = stepper.idx(i);

        if let Assembly::Instruction(AsmInstruction::JMP { label }) = instr {
            let mut dest_label = *label;
            let mut dest = stepper.jmp_label(dest_label);
            let mut dest_instr = stepper.idx(stepper.jmp_next_instruction(dest).expect("Label lead into nothing-ness"));
            loop {
                if let Assembly::Instruction(AsmInstruction::JMP { label }) = dest_instr {
                    dest_label = *label;
                    dest = stepper.jmp_label(dest_label);
                    dest_instr = stepper.idx(stepper.jmp_next_instruction(dest).expect("Label lead into nothing-ness"));
                } else {
                    break;
                }
            }

            res.push(Assembly::Instruction(AsmInstruction::JMP { label: dest_label }));
        } else {
            res.push(instr.clone());
        }
        

        i += 1;
    }
}


struct AssemblyStepper {
    assembly: Vec<Assembly>,
    label_idx: Vec<usize>
}

impl AssemblyStepper {
    pub fn new(assembly: Vec<Assembly>) -> Self {
        let max_label = Self::get_max_label(&assembly) + 1;

        let mut label_idx = Vec::with_capacity(max_label);
        label_idx.resize(max_label, 0);

        let mut label_buffer = vec![];
        for i in 0..assembly.len() {
            let instr = &assembly[i];
            match instr {
                Assembly::Label(i) => {
                    label_buffer.push(i);
                }

                Assembly::Instruction(_) => {
                    for label in label_buffer.drain(..) {
                        label_idx[*label] = i;
                    }
                }
            }
        }

        Self {
            assembly,
            label_idx
        }
    }

    fn get_max_label(assembly: &Vec<Assembly>) -> usize {
        let mut max = 0;
        for instr in assembly {
            if let Assembly::Label(i) = instr {
                max = max.max(*i);
            }
        }

        max
    }

    pub fn idx(&self, i: usize) -> &Assembly {
        &self.assembly[i]
    }

    pub fn is_valid(&self, i: usize) -> bool {
        self.assembly.len() > i
    }

    pub fn jmp_next_instruction(&self, mut i: usize) -> Option<usize> {
        loop {
            if self.assembly.len() <= i {
                return None;
            }
            if self.assembly[i].is_instruction() {
                return Some(i);
            }
            i += 1;
        }
    }

    pub fn jmp_label(&self, label: usize) -> usize {
        self.label_idx[label]
    }
    
}