use std::{fs, io::Write};

// To avoid magic numbers in the match statements to come
const SXV: u8 = 0;
const AXV: u8 = 1;
const SON: u8 = 2;
const COP: u8 = 3;
const ADD: u8 = 4;
const SUB: u8 = 5;
const MUL: u8 = 6;
const DIV: u8 = 7;
const JRX: u8 = 8;
const BRZ: u8 = 9;
const BRN: u8 = 10;
const BRP: u8 = 11;
const PUS: u8 = 12;
const POP: u8 = 13;
const RIN: u8 = 14;
const OUT: u8 = 15;
const RAM_SIZE: usize = 4096;
const ROM_SIZE: usize = 16;
const ROM_SIZE_AS_BYTES: usize = 24;
const STACK_POINTER_START: usize = 0xFFF;
const PROGRAM_COUNTER_START: usize = 0x000;
const HALT_SEQUENCE_LENGTH: u8 = 3;

// Representation of a Fortis machine 
#[derive(Debug)]
pub struct Machine {
    rom: [u16; ROM_SIZE],
    ram: [u8; RAM_SIZE], // Holds instructions and the stack
    r1: u8, // Register 1
    r2: u8, // Register 2
    // usize because it is easier to access ram. They are wrapped in the execution 
    stack_pointer: usize,
    program_counter: usize,
    previous_inst: u8,
    reads_next: bool, // Used for Next-Value Instructions
    halt: u8, // Increments for each consecutive 0x0 instruction
}
impl Machine {
    // Tries to create a new Fortis machine using the given Fortis machine code file path
    pub fn new(file_path: String) -> Result<Self, Box<dyn std::error::Error + 'static>> {
        let fp = fs::read(file_path)?;
        // Since each ROM cell is 12 bits, every 3 bytes contain 2 ROM cells.
        // So here we go through every 3 bytes and map then into two u16
        // (since there is no u12 in Rust) 
        let rom = fp[0..ROM_SIZE_AS_BYTES]
            .chunks(3)
            .flat_map(|b| {
                let (b0, b1, b2) = (b[0] as u16, b[1] as u16, b[2] as u16);
                [b0 | ((b1 & 0xf) << 8), ((b1 & 0xf0) >> 4) | (b2 << 4)]
            })
            .collect::<Vec<u16>>()
            .try_into()
            .unwrap();

        let mut ram = [0; RAM_SIZE];
        ram[..fp[ROM_SIZE_AS_BYTES..].len()].clone_from_slice(&fp[ROM_SIZE_AS_BYTES..]);
        
        Ok(Self {
            rom,
            ram,
            r1: 0,
            r2: 0,
            stack_pointer: STACK_POINTER_START,
            program_counter: PROGRAM_COUNTER_START,
            previous_inst: 0,
            reads_next: false,
            halt: 0,
        })
    }

    // One CPU cycle (Note: Next-Value and Jump-Branch instructions takes 2 cycles to execute)
    pub fn cycle(&mut self) -> Result<&mut Self, &'static str> {
        let inst = self.at_program_counter();

        // Halt sequence detector
        self.halt = if inst == 0 { self.halt + 1 } else { 0 };
        if self.halt >= HALT_SEQUENCE_LENGTH {
            return Ok(self);
        }

        self.program_counter += 1;
        self.program_counter %= RAM_SIZE;

        // Previous instruction was a Next-Value or Jump-Branch
        // So the machine uses the current instruction value
        // as a parameter to the previous instruction
        if self.reads_next {
            self.reads_next = false;
            return match self.previous_inst {
                SXV => self.sxv(inst),
                AXV => self.axv(inst),
                SON => self.son(inst),
                JRX => self.jrx(inst),
                BRZ => self.brz(inst),
                BRN => self.brn(inst),
                BRP => self.brp(inst),
                _ => panic!("Undefined"),
            };
        }

        match inst {
            // Next-Value or Jump-Branch
            SXV | AXV | SON | JRX | BRZ | BRN | BRP => {
                self.reads_next = true;
                self.previous_inst = inst;
                return Ok(self);
            }

            COP => self.cop(),
            ADD => self.add(),
            SUB => self.sub(),
            MUL => self.mul(),
            DIV => self.div(),
            PUS => self.pus(),
            POP => self.pop(),
            RIN => self.rin(),
            OUT => self.out(),
            _ => Err("Unknown instruction"),
        }
    }

    // Run the machine until it halts
    pub fn run(&mut self) {
        while self.halt < HALT_SEQUENCE_LENGTH {
            if let Err(s) = self.cycle() {
                println!("Error: {}", s);
            }
        }
    }

    // Returns the instruction at the machine's program counter
    #[inline(always)]
    pub fn at_program_counter(&self) -> u8 {
        let memory_cell = self.ram[self.program_counter >> 1];
        if self.program_counter & 1 == 0 {
            memory_cell & 0xf
        } else {
            memory_cell >> 4
        }
    }

    /* === Instructions as Functions === */

    #[inline(always)]
    fn sxv(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        self.r1 = inst;
        Ok(self)
    }

    #[inline(always)]
    fn axv(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        // Sign extend u4 -> u8
        let sign_extend = if inst >> 3 == 1 {0xf0 | inst} else {inst};
        self.r1 = self.r1.wrapping_add(sign_extend);
        Ok(self)
    }

    #[inline(always)]
    fn son(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        self.r1 = (self.r1 << 4) | inst;
        Ok(self)
    }

    #[inline(always)]
    fn cop(&mut self) -> Result<&mut Self, &'static str> {
        self.r2 = self.r1;
        Ok(self)
    }

    #[inline(always)]
    fn add(&mut self) -> Result<&mut Self, &'static str> {
        self.r1 = self.r1.wrapping_add(self.r2);
        Ok(self)
    }

    #[inline(always)]
    fn sub(&mut self) -> Result<&mut Self, &'static str> {
        self.r1 = self.r1.wrapping_sub(self.r2);
        Ok(self)
    }

    #[inline(always)]
    fn mul(&mut self) -> Result<&mut Self, &'static str> {
        self.r1 = self.r1.wrapping_mul(self.r2);
        Ok(self)
    }

    #[inline(always)]
    fn div(&mut self) -> Result<&mut Self, &'static str> {
        if self.r2 == 0 {
            return Err("Division by zero");
        }
        self.r1 /= self.r2;
        Ok(self)
    }

    #[inline(always)]
    fn jrx(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        self.program_counter = self.rom[inst as usize] as usize;
        Ok(self)
    }

    #[inline(always)]
    fn brz(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        if self.r1 == 0 {
            self.program_counter = self.rom[inst as usize] as usize;
        }
        Ok(self)
    }

    #[inline(always)]
    fn brn(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        // Negative numbers will have signed bit on, therefore they are greater than 0x80 in the unsigned world
        if self.r1 >= 0b1000_0000 {
            self.program_counter = self.rom[inst as usize] as usize;
        }
        Ok(self)
    }

    #[inline(always)]
    fn brp(&mut self, inst: u8) -> Result<&mut Self, &'static str> {
        // Positive numbers will have the signed bit off, therefore they are less than 0x80 in the unsigned world
        if self.r1 < 0b1000_0000 && self.r1 != 0 {
            self.program_counter = self.rom[inst as usize] as usize;
        }
        Ok(self)
    }

    #[inline(always)]
    fn pus(&mut self) -> Result<&mut Self, &'static str> {
        self.ram[self.stack_pointer] = self.r1;
        self.stack_pointer = self.stack_pointer.wrapping_sub(1) % RAM_SIZE;
        Ok(self)
    }

    #[inline(always)]
    fn pop(&mut self) -> Result<&mut Self, &'static str> {
        self.stack_pointer += 1;
        self.stack_pointer %= RAM_SIZE;
        self.r1 = self.ram[self.stack_pointer];
        Ok(self)
    }

    // To "emulate" the machine the emulator reads the first character of each line
    // (This was done to avoid termios and the platform dependency rabbit hole)
    #[inline(always)]
    fn rin(&mut self) -> Result<&mut Self, &'static str> {
        // Flushes stdout buffer to avoid reading before displaying
        if let Err(_) = std::io::stdout().flush() {
            return Err("Error flushing stdout");
        }
        let mut buf = String::new();
        if let Err(_) = std::io::stdin().read_line(&mut buf) {
            return Err("Error reading from stdin");
        }
        if let Some(b) = buf.bytes().next() {
            self.r1 = b;
        } else {
            return Err("Nothing was read");
        }
        Ok(self)
    }

    #[inline(always)]
    fn out(&mut self) -> Result<&mut Self, &'static str> {
        print!("{}", self.r1 as char);
        Ok(self)
    }
}
