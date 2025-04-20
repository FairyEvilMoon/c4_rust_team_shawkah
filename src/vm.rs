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
    Mul = 6,
    Div = 7,
    Mod = 8,
    Jmp = 9,
    Jz = 10,
    Jnz = 11,
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
            6 => Ok(Instruction::Mul),
            7 => Ok(Instruction::Div),
            8 => Ok(Instruction::Mod),
            9 => Ok(Instruction::Jmp),
            10 => Ok(Instruction::Jz),
            11 => Ok(Instruction::Jnz),
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
    InvalidJumpTarget(Value),
    DivisionByZero,
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
    code_size: usize, // Store code size for jump validation
}

impl VirtualMachine {
    pub fn new(code: Vec<Value>) -> Self {
        let stack = vec![0; DEFAULT_MEM_SIZE];
        let initial_sp = stack.len();
        let code_size = code.len(); // Store code size on creation
        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp,
            ax: 0,
            code,
            stack,
            running: false,
            code_size, // Initialize the field
        }
    }

    /// Fetches the next value from code memory (instruction or operand). Advances PC.
    fn fetch_value(&mut self) -> Result<Value, VmError> {
        // Use stored code_size for bounds check
        if self.pc >= self.code_size {
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

    /// Validates a jump target address, ensuring it's within the code section bounds.
    fn validate_jump_target(&self, target: Value) -> Result<usize, VmError> {
        // Check target is non-negative and strictly less than code_size
        if target < 0 || (target as usize) >= self.code_size {
            Err(VmError::InvalidJumpTarget(target))
        } else {
            Ok(target as usize)
        }
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
                self.running = false;
            }
            Instruction::Add => {
                let operand = self.pop()?;
                self.ax = operand.checked_add(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Sub => {
                let operand = self.pop()?;
                self.ax = operand.checked_sub(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Mul => {
                let operand = self.pop()?;
                self.ax = operand.checked_mul(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Div => {
                let divisor = self.ax;
                if divisor == 0 {
                    return Err(VmError::DivisionByZero);
                }
                let dividend = self.pop()?;
                // Perform division: dividend (from stack) / divisor (from ax)
                self.ax = dividend.checked_div(divisor).ok_or(VmError::ArithmeticOverflow)?; // Overflow with MIN / -1
            }
            Instruction::Mod => {
                let divisor = self.ax;
                if divisor == 0 {
                    return Err(VmError::DivisionByZero);
                }
                let dividend = self.pop()?;
                 // Perform modulo: dividend (from stack) % divisor (from ax)
                self.ax = dividend.checked_rem(divisor).ok_or(VmError::ArithmeticOverflow)?; // Overflow with MIN % -1
            }
            Instruction::Jmp => {
                let target_addr = self.fetch_operand()?;
                self.pc = self.validate_jump_target(target_addr)?;
            }
            Instruction::Jz => {
                let target_addr = self.fetch_operand()?;
                if self.ax == 0 {
                    self.pc = self.validate_jump_target(target_addr)?;
                }
            }
            Instruction::Jnz => {
                let target_addr = self.fetch_operand()?;
                 if self.ax != 0 {
                    self.pc = self.validate_jump_target(target_addr)?;
                 }
            }
        }
        Ok(())
    }
}