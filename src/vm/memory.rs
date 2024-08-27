pub struct Memory {
    instruction_mem: [u8; 2048],
    data_mem: [u8; 256],
    call_stack: [usize; 16],
    csp: usize,
    gp_stack: [u8; 64],
    sp: usize,
}

impl Memory {
    pub fn with_rom(rom: [u8; 2048]) -> Self {
        Self {
            instruction_mem: rom,
            data_mem: [0; 256],
            call_stack: [0; 16],
            csp: 0,
            gp_stack: [0; 64],
            sp: 0
        }
    }
    #[inline(always)]
    pub fn fetch_instruction(&self, instruction_pointer: usize) -> u8 {
        self.instruction_mem[instruction_pointer].to_owned()
    }
    #[inline(always)]
    pub fn read_data(&self, data_pointer: usize) -> u8 {
        self.data_mem[data_pointer].to_owned()
    }
    #[inline(always)]
    pub fn write_data(&mut self, data_pointer: usize, data: u8) {
        self.data_mem[data_pointer] = data;
    }
    #[inline(always)]
    pub fn call(&mut self, instruction_pointer: usize) {
        self.call_stack[self.csp] = instruction_pointer;
        self.csp += 1;
    }
    #[inline(always)]
    pub fn ret(&mut self) -> usize {
        self.csp -= 1;
        return self.call_stack[self.csp];
    }

    pub fn push(&mut self, data: u8) {
        self.gp_stack[self.sp] = data;
        self.sp += 1;
    }

    pub fn pop(&mut self) -> u8 {
        self.sp -= 1;
        return self.gp_stack[self.sp];
    }
}