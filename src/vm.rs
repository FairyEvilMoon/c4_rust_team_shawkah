//! Virtual Machine implementation for the C4 subset compiler.

use std::convert::TryFrom;

pub type Value = i32;

const DEFAULT_MEM_SIZE: usize = 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum Instruction {
    Nop,
    Imm,
    Push,
    Exit,
}

// Keep pub
impl TryFrom<i32> for Instruction {
    type Error = VmError;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Instruction::Nop),
            1 => Ok(Instruction::Imm),
            2 => Ok(Instruction::Push),
            3 => Ok(Instruction::Exit),
            _ => Err(VmError::InvalidInstruction(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError { 
    InvalidInstruction(Value),
    PcOutOfBounds,
}

pub struct VirtualMachine {

    pub pc: usize,
    pub sp: usize, 
    pub bp: usize,
    pub ax: Value,


    code: Vec<Value>,
    pub stack: Vec<Value>, 

    running: bool, // Internal state, likely keep private
}

impl VirtualMachine {

    pub fn new(code: Vec<Value>) -> Self {
        let stack = vec![0; DEFAULT_MEM_SIZE];
        let initial_sp = stack.len();
        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp,
            ax: 0,
            code,
            stack,
            running: false,
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        self.running = true;

        while self.running {
            let instruction_value = self.fetch()?;
            let instruction = Instruction::try_from(instruction_value)?;
            self.execute(instruction)?;
        }

        Ok(())
    }

    // Internal method, keep private
    fn fetch(&mut self) -> Result<Value, VmError> {
        if self.pc >= self.code.len() {
            self.running = false;
            Err(VmError::PcOutOfBounds)
        } else {
            let instruction_value = self.code[self.pc];

            self.pc += 1;
            Ok(instruction_value)
        }
    }

    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
        match instruction {
            Instruction::Nop => {}
            Instruction::Exit => {
                self.running = false;
            }
            _ => {
                self.running = false; // Stop the loop
                return Err(VmError::InvalidInstruction(instruction as i32));
            }
        }
        Ok(())
    }
}