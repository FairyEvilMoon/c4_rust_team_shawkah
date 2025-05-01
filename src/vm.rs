use std::convert::TryFrom;

pub type Value = i32;
pub const DEFAULT_MEM_SIZE: usize = 1024 * 1024; // Example stack size

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
    Call = 12,
    Ent = 13,
    Adj = 14,
    Lev = 15,
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
            // Function Call Instructions
            12 => Ok(Instruction::Call),
            13 => Ok(Instruction::Ent),
            14 => Ok(Instruction::Adj),
            15 => Ok(Instruction::Lev),
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
    // Add more specific errors if needed
}

#[derive(Debug, PartialEq)]
pub struct VirtualMachine {
    pub pc: usize,          // Program Counter
    pub sp: usize,          // Stack Pointer (points to the *next* free slot, stack grows down)
    pub bp: usize,          // Base Pointer (points to the base of the current stack frame)
    pub ax: Value,          // Accumulator Register
    code: Vec<Value>,       // Code segment
    pub stack: Vec<Value>,  // Stack segment
    running: bool,
    code_size: usize,       // Store code size for jump validation
}

impl VirtualMachine {
    pub fn new(code: Vec<Value>) -> Self {
        let stack = vec![0; DEFAULT_MEM_SIZE];
        let initial_sp = stack.len(); // SP starts at the top (end) of the stack vector
        let code_size = code.len();
        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp, // Initially, BP is the same as SP
            ax: 0,
            code,
            stack,
            running: false,
            code_size,
        }
    }

    /// Fetches the next value from code memory (instruction or operand). Advances PC.
    fn fetch_value(&mut self) -> Result<Value, VmError> {
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
            VmError::PcOutOfBounds => VmError::OperandExpected,
            other => other,
        })
    }

    /// Pushes a value onto the stack. Decrements SP.
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 {
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1; // Stack grows downwards
            self.stack[self.sp] = value;
            Ok(())
        }
    }

    /// Pops a value from the stack. Increments SP.
    fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= self.stack.len() { // Check if SP is at or beyond the initial top
            Err(VmError::StackUnderflow)
        } else {
            let value = self.stack[self.sp];
            self.sp += 1;
            Ok(value)
        }
    }

    /// Validates a jump target address.
    fn validate_jump_target(&self, target: Value) -> Result<usize, VmError> {
        if target < 0 || (target as usize) >= self.code_size {
            Err(VmError::InvalidJumpTarget(target))
        } else {
            Ok(target as usize)
        }
    }

    pub fn run(&mut self) -> Result<(), VmError> {
        self.running = true;
        while self.running {
            let instruction = self.fetch_instruction()?;
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
                self.running = false;
                println!("VM Exit. Final AX: {}", self.ax); // Helpful for debugging
            }
            Instruction::Add => {
                let operand = self.pop()?;
                self.ax = operand.checked_add(self.ax).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Sub => {
                let operand = self.pop()?;
                // Order matters: operand (top of stack) - self.ax
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
                // Order: dividend (stack) / divisor (ax)
                self.ax = dividend.checked_div(divisor).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Mod => {
                let divisor = self.ax;
                if divisor == 0 {
                    return Err(VmError::DivisionByZero);
                }
                let dividend = self.pop()?;
                // Order: dividend (stack) % divisor (ax)
                self.ax = dividend.checked_rem(divisor).ok_or(VmError::ArithmeticOverflow)?;
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

            // === Function Call Instructions ===

            Instruction::Call => {
                // Operand: target function address
                let target_addr = self.fetch_operand()?;
                let target_pc = self.validate_jump_target(target_addr)?;
                // Push the return address (PC of *next* instruction) onto the stack
                self.push(self.pc as Value)?;
                // Jump to the target function
                self.pc = target_pc;
            }

            Instruction::Ent => {
                // Operand: size of local variables for the new frame
                let locals_size = self.fetch_operand()?;
                if locals_size < 0 {
                    // Or handle as appropriate for C4 (maybe allows 0?)
                    return Err(VmError::InvalidInstruction(locals_size)); // Or a more specific error
                }
                // Push the old base pointer onto the stack
                self.push(self.bp as Value)?;
                // Set the new base pointer to the current stack pointer
                self.bp = self.sp;
                // Allocate space for locals by decrementing stack pointer
                // Check for potential overflow (sp wrapping around below 0)
                if self.sp < (locals_size as usize) {
                    return Err(VmError::StackOverflow);
                }
                self.sp -= locals_size as usize;
            }

            Instruction::Adj => {
                // Operand: number of arguments caller pushed before CALL
                let arg_count = self.fetch_operand()?;
                if arg_count < 0 {
                     return Err(VmError::InvalidInstruction(arg_count)); // Or specific error
                }
                // Remove arguments from stack by incrementing stack pointer
                let new_sp = self.sp.checked_add(arg_count as usize);
                match new_sp {
                    Some(sp) if sp <= self.stack.len() => self.sp = sp, // Check against original stack size
                    _ => return Err(VmError::StackUnderflow), // Trying to adjust past original top
                }
            }

            Instruction::Lev => { // Leave subroutine (typically the last instruction)
                // Restore the caller's stack pointer (discarding locals and saved BP)
                self.sp = self.bp;
                // Restore the caller's base pointer by popping it from the stack
                self.bp = self.pop()? as usize; // Assuming BP was pushed as Value
                // Restore the program counter by popping the return address
                self.pc = self.pop()? as usize; // Assuming return PC was pushed as Value
            }
        }
        Ok(())
    }
}