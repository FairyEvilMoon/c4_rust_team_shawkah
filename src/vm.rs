use std::convert::TryFrom;

pub type Value = i32;
pub const DEFAULT_MEM_SIZE: usize = 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum Instruction {
    Nop = 0,
    Imm = 1,
    Push = 2,
    Exit = 3,
    Add = 4,
    Sub = 5,
}

impl TryFrom<i32> for Instruction {
    type Error = VmError;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Instruction::Nop),
            1 => Ok(Instruction::Imm),
            2 => Ok(Instruction::Push),
            3 => Ok(Instruction::Exit),
            4 => Ok(Instruction::Add),
            5 => Ok(Instruction::Sub),
            _ => Err(VmError::InvalidInstruction(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    InvalidInstruction(Value),
    PcOutOfBounds,
    StackOverflow,
    StackUnderflow,
    OperandExpected,
    ArithmeticOverflow,
}

#[derive(Debug, PartialEq)]
pub struct VirtualMachine {
    pub pc: usize,
    pub sp: usize,
    pub bp: usize,
    pub ax: Value,
    code: Vec<Value>,
    pub stack: Vec<Value>,
    running: bool,
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

    /// Fetches the next value from code memory (instruction or operand). Advances PC.
    fn fetch_value(&mut self) -> Result<Value, VmError> {
        if self.pc >= self.code.len() {
            self.running = false;
            Err(VmError::PcOutOfBounds)
        } else {
            let value = self.code[self.pc];
            self.pc += 1;
            Ok(value)
        }
    }

    /// Fetches specifically an instruction.
    fn fetch_instruction(&mut self) -> Result<Instruction, VmError> {
        let value = self.fetch_value()?;
        Instruction::try_from(value)
    }

    /// Fetches specifically an operand.
    fn fetch_operand(&mut self) -> Result<Value, VmError> {
        self.fetch_value().map_err(|e| match e {
            VmError::PcOutOfBounds => VmError::OperandExpected, // More specific error
            other => other,
        })
    }

    /// Pushes a value onto the stack. Decrements SP.
    pub(crate) fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 {
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1; // Stack grows downwards
            self.stack[self.sp] = value;
            Ok(())
        }
    }

    /// Pops a value from the stack. Increments SP.
    pub(crate) fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= self.stack.len() { // Use stack.len() for underflow check
            Err(VmError::StackUnderflow)
        } else {
            let value = self.stack[self.sp];
            self.sp += 1; // Stack grows downwards, so popping increases SP
            Ok(value)
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        self.running = true;
        while self.running {
            let instruction = self.fetch_instruction()?; // Fetch first
            // Use ? to propagate errors from execute immediately
            self.execute(instruction)?;
            // Check running flag again in case EXIT occurred
            if !self.running {
                break;
            }
        }
        Ok(())
    }

    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
        match instruction {
            Instruction::Nop => { /* Do nothing */ }
            Instruction::Imm => {
                self.ax = self.fetch_operand()?;
            }
            Instruction::Push => {
                self.push(self.ax)?;
            }
            Instruction::Exit => {
                self.running = false; // Signal run loop to stop
            }
            Instruction::Add => {
                let operand = self.pop()?; // Pop RHS (value previously pushed)
                // ax = operand (from stack) + ax (current value)
                // Use checked_add for overflow detection
                self.ax = operand.checked_add(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Sub => {
                let operand = self.pop()?; // Pop RHS (value previously pushed)
                // ax = operand (from stack) - ax (current value)
                // Use checked_sub for overflow detection
                self.ax = operand.checked_sub(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
        }
        Ok(())
    }
}