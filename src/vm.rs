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
    Li = 16,  // Load Int: Pop address, push stack[addr]
    Lc = 17,  // Load Char: Pop address, push stack[addr] & 0xFF
    Si = 18,  // Store Int: Pop address, pop value, stack[addr] = value
    Sc = 19,  // Store Char: Pop address, pop value, stack[addr] = value & 0xFF
    // --- Add others later if needed ---
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
            12 => Ok(Instruction::Call),
            13 => Ok(Instruction::Ent),
            14 => Ok(Instruction::Adj),
            15 => Ok(Instruction::Lev),
            16 => Ok(Instruction::Li),
            17 => Ok(Instruction::Lc),
            18 => Ok(Instruction::Si),
            19 => Ok(Instruction::Sc),
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
    MemoryAccessOutOfBounds(Value), // New error for invalid addresses
}

#[derive(Debug, PartialEq)]
pub struct VirtualMachine {
    pub pc: usize,
    pub sp: usize,
    pub bp: usize,
    pub ax: Value,
    code: Vec<Value>,
    pub stack: Vec<Value>, // Stack also serves as general memory for LI/LC/SI/SC
    running: bool,
    code_size: usize,
}

impl VirtualMachine {
    pub fn new(code: Vec<Value>) -> Self {
        let stack = vec![0; DEFAULT_MEM_SIZE];
        let initial_sp = stack.len();
        let code_size = code.len();
        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp,
            ax: 0,
            code,
            stack,
            running: false,
            code_size,
        }
    }

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

    fn fetch_instruction(&mut self) -> Result<Instruction, VmError> {
        let value = self.fetch_value()?;
        Instruction::try_from(value)
    }

    fn fetch_operand(&mut self) -> Result<Value, VmError> {
        self.fetch_value().map_err(|e| match e {
            VmError::PcOutOfBounds => VmError::OperandExpected,
            other => other,
        })
    }

    fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 {
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1;
            self.stack[self.sp] = value;
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= self.stack.len() {
            Err(VmError::StackUnderflow)
        } else {
            let value = self.stack[self.sp];
            self.sp += 1;
            Ok(value)
        }
    }

    /// Validates an address for memory access (within stack bounds).
    fn validate_memory_address(&self, addr: Value) -> Result<usize, VmError> {
        if addr < 0 || (addr as usize) >= self.stack.len() {
            Err(VmError::MemoryAccessOutOfBounds(addr))
        } else {
            Ok(addr as usize)
        }
    }

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
                println!("VM Exit. Final AX: {}", self.ax);
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
                if divisor == 0 { return Err(VmError::DivisionByZero); }
                let dividend = self.pop()?;
                self.ax = dividend.checked_div(divisor).ok_or(VmError::ArithmeticOverflow)?;
            }
            Instruction::Mod => {
                let divisor = self.ax;
                if divisor == 0 { return Err(VmError::DivisionByZero); }
                let dividend = self.pop()?;
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
            Instruction::Call => {
                let target_addr = self.fetch_operand()?;
                let target_pc = self.validate_jump_target(target_addr)?;
                self.push(self.pc as Value)?; // Push return address
                self.pc = target_pc;
            }
            Instruction::Ent => {
                let locals_size = self.fetch_operand()?;
                if locals_size < 0 { return Err(VmError::InvalidInstruction(locals_size)); } // Or specific error
                self.push(self.bp as Value)?; // Push old base pointer
                self.bp = self.sp;            // Set new base pointer
                if self.sp < (locals_size as usize) { return Err(VmError::StackOverflow); }
                self.sp -= locals_size as usize; // Allocate space for locals
            }
            Instruction::Adj => {
                let arg_count = self.fetch_operand()?;
                if arg_count < 0 { return Err(VmError::InvalidInstruction(arg_count)); }
                let new_sp = self.sp.checked_add(arg_count as usize);
                match new_sp { // Remove arguments pushed by caller
                    Some(sp) if sp <= self.stack.len() => self.sp = sp,
                    _ => return Err(VmError::StackUnderflow),
                }
            }
            Instruction::Lev => {
                self.sp = self.bp;            // Restore SP, discarding locals & saved BP
                self.bp = self.pop()? as usize; // Restore caller's BP
                self.pc = self.pop()? as usize; // Restore caller's PC (return address)
            }

            // === Memory Access Instructions ===

            Instruction::Li => {
                // Load Integer from address specified by AX into AX
                // Original C4: *(int *)ax = *(int *)sp++; AX = *(int*)AX;
                let addr = self.validate_memory_address(self.ax)?; // Use AX as address
                self.ax = self.stack[addr]; // Load value from memory into AX
            }
            Instruction::Lc => {
                // Load Character from address specified by AX into AX (zero-extended)
                // Original C4: *(char *)ax = *(char *)sp++; AX = *(char*)AX;
                let addr = self.validate_memory_address(self.ax)?; // Use AX as address
                let value = self.stack[addr]; // Read the full Value (i32)
                self.ax = value & 0xFF; // Mask to get the lower byte, zero-extend
            }
            Instruction::Si => {
                // Store Integer from stack top into address specified by AX
                // Original C4: *(int *)ax = *sp++;
                let value_to_store = self.pop()?; // Value is on top of stack
                let addr = self.validate_memory_address(self.ax)?; // Address is in AX
                self.stack[addr] = value_to_store; // Store the value
            }
            Instruction::Sc => {
                // Store Character from stack top into address specified by AX
                // Original C4: *(char *)ax = *sp++;
                let value_to_store = self.pop()?; // Value is on top of stack
                let addr = self.validate_memory_address(self.ax)?; // Address is in AX
                self.stack[addr] = value_to_store & 0xFF; // Store only the lower byte
            }
        }
        Ok(())
    }
}