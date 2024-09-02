use core::panic;
use std::time::Instant;

use super::memory::Memory;


pub struct CPU {
    flag_z: bool,
    flag_c: bool,
    mem: Memory,
    regs: [u8; 16], //reg 0 is always 0
    ip: usize,
    halt: bool,
}


impl CPU {

    pub fn with_rom(rom: [u8; 2048]) -> Self {
        Self { 
            flag_z: false, 
            flag_c: false, 
            mem: Memory::with_rom(rom), 
            regs: [0; 16], 
            ip: 0, 
            halt: false
        }
    }


    ///Gets the registries used registries from the instruction. a, b -> &mut c
    #[inline(always)]
    pub fn get_regs(&mut self) -> (u8, u8, &mut u8) {
        let a = self.mem.fetch_instruction(self.ip);
        let b_c = self.mem.fetch_instruction(self.ip + 1);

        let a = a & 0b0000_1111;
        let b = b_c >> 4;
        let c = b_c & 0b0000_1111;

        let a = self.regs[a as usize];
        let b = self.regs[b as usize];
        let c = &mut self.regs[c as usize];
        return (a, b, c);
    }

    #[inline(always)]
    ///Returns address, index of b
    pub fn get_ab_offset(&mut self) -> (u8, u8) {
        let a = self.mem.fetch_instruction(self.ip);
        let b_o = self.mem.fetch_instruction(self.ip + 1);

        let a = a & 0b0000_1111;
        let b = b_o >> 4;

        let a = self.regs[a as usize];
        let adr = (((b_o as i8) << 4) >> 4) as u8 + a;
        return (adr, b);
    }

    #[inline(always)]
    pub fn get_cond_addr(&mut self) -> (u8, usize) {
        let cond = self.mem.fetch_instruction(self.ip) & 0b0000_1111; 
        let adr = (((cond & 0b0000_0011) as usize) << 8) | self.mem.fetch_instruction(self.ip + 1) as usize;

        return (cond >> 2, adr << 1);
    }
    #[inline(always)]
    pub fn get_a_immediate(&mut self) -> (&mut u8, u8) {
        let a = self.mem.fetch_instruction(self.ip);
        let immediate = self.mem.fetch_instruction(self.ip + 1);

        let a = a & 0b0000_1111;

        let a = &mut self.regs[a as usize];
        return (a, immediate);
    }
    pub fn run(&mut self) {
        let cycle_start_time = Instant::now();
        let mut instruction_amt: u64 = 0;

        while !self.halt {
            
            let first_bit = self.mem.fetch_instruction(self.ip);
            let instruction = first_bit >> 4;
            match instruction {
                0 => {
                    //NOP
                    self.ip += 2;
                }
                1 => { //Hlt
                    self.halt = true;

                    self.ip += 2;

                }
                2 => { //Add
                    let (a, b, c) = self.get_regs();
                    let (res, overflowing) = a.overflowing_add(b);
                    *c = res;
                    self.flag_c = overflowing;
                    self.flag_z = res == 0;

                    self.ip += 2;

                }
                3 => { //Sub
                    let (a, b, c) = self.get_regs();
                    let (res, overflowing) = a.overflowing_sub(b);
                    *c = res;
                    self.flag_c = overflowing;
                    self.flag_z = res == 0;

                    self.ip += 2;

                }
                4 => { //Nor
                    let (a, b, c) = self.get_regs();
                    *c = !(a | b);
                    self.flag_z = *c == 0;
                    self.flag_c = false;

                    self.ip += 2;

                }
                5 => { //And
                    let (a, b, c) = self.get_regs();
                    *c = a & b;
                    self.flag_z = *c == 0;
                    self.flag_c = false;

                    self.ip += 2;

                }
                6 => { //XOR
                    let (a, b, c) = self.get_regs();
                    *c = a ^ b;
                    self.flag_z = *c == 0;
                    self.flag_c = false;

                    self.ip += 2;

                }
                7 => { //RSH
                    let (a, _, c) = self.get_regs();
                    *c = a >> 1;

                    self.ip += 2;

                }
                8 => { //LDI
                    let (a, immediate) = self.get_a_immediate();
                    *a = immediate;

                    self.ip += 2;

                }
                9 => { //ADI
                    let (a, immediate) = self.get_a_immediate();
                    let (res, overflowing) = a.overflowing_add(immediate);
                    *a = res;
                    self.flag_c = overflowing;
                    self.flag_z = res == 0;

                    self.ip += 2;

                }
                10 => { //JMP
                    let (_, adr) = self.get_cond_addr();
                    
                    self.ip = adr;
                }

                11 => { //brh
                    let (cond, adr) = self.get_cond_addr();
                    let do_jump = match cond {
                        0 => self.flag_z,
                        1 => !self.flag_z,
                        2 => self.flag_c,
                        3 => !self.flag_c,
                        _ => panic!("Invalid condition (This should NEVER be the case)")
                    };
                    if do_jump {
                        self.ip = adr;
                    } else {
                        self.ip += 2;
                    }
                }
                12 => { //call
                    let (_, adr) = self.get_cond_addr();
                    self.mem.call(self.ip);
                    self.ip = adr;
                }
                13 => { //ret
                    self.ip = self.mem.ret() + 2;
                }
                14 => { //LOD
                    let (mem_adr, b) = self.get_ab_offset(); //This cannot return a mutable reference to b, as this would lead to a borrow issue
                    self.regs[b as usize] = self.mem.read_data(mem_adr as usize);

                    self.ip += 2;

                }
                15 => { //Str
                    let (mem_adr, b) = self.get_ab_offset(); 
                    self.mem.write_data(mem_adr as usize, self.regs[b as usize]);

                    self.ip += 2;

                }
                _ => panic!("Invalid instruction, this should NEVER happen")
            }
            //In case we wrote to r0, clear it
            self.regs[0] = 0;
            instruction_amt += 1;
        }
        println!("{} Instructions in {} sec", instruction_amt, Instant::now().duration_since(cycle_start_time).as_secs_f64())
    }

    
}


