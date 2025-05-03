// src/vm.rs

use std::convert::TryFrom;

// --- Types and Constants ---

pub type Value = i32; // VM uses 32-bit integers
pub const DEFAULT_MEM_SIZE: usize = 1024 * 1024; // 1MB default for stack/heap
// Stack grows downwards from high memory in this model
pub const STACK_TOP_ADDR: usize = DEFAULT_MEM_SIZE;

// --- Instructions ---
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum Instruction {
    // Core & Flow Control
    Nop = 0, Imm = 1, Push = 2, Exit = 3,
    // Arithmetic (Pop() op AX) -> AX = Op1 op Op2
    Add = 4, Sub = 5, Mul = 6, Div = 7, Mod = 8,
    // Control Flow
    Jmp = 9, Jz = 10, Jnz = 11,
    // Function Call Mechanism
    Call = 12, Ent = 13, Adj = 14, Lev = 15,
    // Memory Access
    Li = 16, Lc = 17, Si = 18, Sc = 19, Lea = 20, // Load Integer, Load Char, Store Int, Store Char, Load Effective Address
    // Bitwise/Logical (Pop() op AX) -> AX = Op1 op Op2
    Or  = 21, Xor = 22, And = 23, Shl = 24, Shr = 25,
    // Relational (Pop() op AX) -> AX = (Op1 op Op2) ? 1 : 0
    Eq = 26, Ne = 27, Lt = 28, Gt = 29, Le = 30, Ge = 31,
    // Unary
    Neg = 32, Not = 33, // Negate, Logical NOT (!=0 -> 0, ==0 -> 1)

    // Potential additions if needed: BNot (Bitwise NOT ~)
}

// --- Instruction TryFrom<Value> ---
// (Identical to your provided code - assuming it's correct)
impl TryFrom<Value> for Instruction {
    type Error = VmError;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Instruction::Nop), 1 => Ok(Instruction::Imm), 2 => Ok(Instruction::Push),
            3 => Ok(Instruction::Exit), 4 => Ok(Instruction::Add), 5 => Ok(Instruction::Sub),
            6 => Ok(Instruction::Mul), 7 => Ok(Instruction::Div), 8 => Ok(Instruction::Mod),
            9 => Ok(Instruction::Jmp), 10 => Ok(Instruction::Jz), 11 => Ok(Instruction::Jnz),
            12 => Ok(Instruction::Call), 13 => Ok(Instruction::Ent), 14 => Ok(Instruction::Adj),
            15 => Ok(Instruction::Lev), 16 => Ok(Instruction::Li), 17 => Ok(Instruction::Lc),
            18 => Ok(Instruction::Si), 19 => Ok(Instruction::Sc), 20 => Ok(Instruction::Lea),
            21 => Ok(Instruction::Or), 22 => Ok(Instruction::Xor), 23 => Ok(Instruction::And),
            24 => Ok(Instruction::Shl), 25 => Ok(Instruction::Shr),
            26 => Ok(Instruction::Eq), 27 => Ok(Instruction::Ne), 28 => Ok(Instruction::Lt),
            29 => Ok(Instruction::Gt), 30 => Ok(Instruction::Le), 31 => Ok(Instruction::Ge),
            32 => Ok(Instruction::Neg), 33 => Ok(Instruction::Not),
            _ => Err(VmError::InvalidInstruction(value)),
        }
    }
}


// --- Errors ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    InvalidInstruction(Value),
    PcOutOfBounds,
    StackOverflow,
    StackUnderflow,
    OperandExpected,
    ArithmeticError(String), // Combine overflow, div-by-zero, etc.
    InvalidJumpTarget(Value),
    MemoryAccessOutOfBounds { address: Value, memory_type: String }, // Clarify which memory
    InvalidShiftAmount(Value), // For SHL/SHR
    DataSegmentAccessError(String), // For printf string reading
    InvalidSyscall(Value), // If using dedicated syscall instruction
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         match self {
            VmError::InvalidInstruction(v) => write!(f, "Invalid instruction opcode: {}", v),
            VmError::PcOutOfBounds => write!(f, "Program counter out of code bounds"),
            VmError::StackOverflow => write!(f, "Stack overflow"),
            VmError::StackUnderflow => write!(f, "Stack underflow"),
            VmError::OperandExpected => write!(f, "Operand expected, but reached end of code"),
            VmError::ArithmeticError(s) => write!(f, "Arithmetic error: {}", s),
            VmError::InvalidJumpTarget(v) => write!(f, "Invalid jump target address in code: {}", v),
            VmError::MemoryAccessOutOfBounds{ address, memory_type } => write!(f, "{} memory access out of bounds at address: {}", memory_type, address),
            VmError::InvalidShiftAmount(v) => write!(f, "Invalid shift amount: {}", v),
            VmError::DataSegmentAccessError(s) => write!(f, "Data Segment access error: {}", s),
            VmError::InvalidSyscall(v) => write!(f, "Invalid syscall number: {}", v),
        }
    }
}
impl std::error::Error for VmError {}


// --- Virtual Machine ---
#[derive(Debug)]
pub struct VirtualMachine {
    // Registers
    pc: usize,          // Program Counter (index into `code`)
    sp: usize,          // Stack Pointer (index into `memory`, points *to top item*, grows down)
    bp: usize,          // Base Pointer (index into `memory`)
    ax: Value,          // Accumulator Register

    // Memory
    code: Vec<Value>,
    memory: Vec<Value>, // Combined stack and (potentially) heap memory (using Value for word-addressing)
    data_segment: Vec<u8>, // Read-only data segment (byte-addressed)

    // State
    running: bool,
    code_size: usize,
    memory_size: usize, // Total size of `memory` in Value words
}

impl VirtualMachine {
    /// Creates a new VM with code, data segment, and default memory size.
    pub fn new(code: Vec<Value>, data_segment: Vec<u8>) -> Self {
        Self::with_memory_size(code, data_segment, DEFAULT_MEM_SIZE)
    }

    /// Creates a new VM with specific memory size.
    pub fn with_memory_size(code: Vec<Value>, data_segment: Vec<u8>, memory_bytes: usize) -> Self {
        // Memory size needs to be in words (Value size)
        let memory_word_size = memory_bytes / std::mem::size_of::<Value>();
        if memory_word_size == 0 {
            panic!("Memory size must be at least {} bytes", std::mem::size_of::<Value>());
        }
        let memory = vec![0; memory_word_size]; // Initialize memory with zeros
        let code_size = code.len();
        let initial_sp = memory_word_size; // SP starts just *past* the end

        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp,
            ax: 0,
            code,
            memory,
            data_segment,
            running: false,
            code_size,
            memory_size: memory_word_size,
        }
    }

    // --- Fetching ---
    #[inline]
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

    #[inline]
    fn fetch_instruction(&mut self) -> Result<Instruction, VmError> {
        let value = self.fetch_value()?;
        Instruction::try_from(value)
    }

    #[inline]
    fn fetch_operand(&mut self) -> Result<Value, VmError> {
        self.fetch_value().map_err(|e| match e {
            VmError::PcOutOfBounds => VmError::OperandExpected,
            other => other,
        })
    }

    // --- Stack Operations (using `memory`) ---
    // SP points *at* the top used slot or memory_size if empty.
    // Push: Decrement SP, write value.
    // Pop: Read value, increment SP.
    #[inline]
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 { // Stack grows down, overflow if SP hits 0
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1;
            // Safety: Bounds check is implicit in sp == 0 check above and sp <= memory_size
            self.memory[self.sp] = value;
            Ok(())
        }
    }

    #[inline]
    fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= self.memory_size { // Underflow if SP is at or beyond the initial top
            Err(VmError::StackUnderflow)
        } else {
            // Safety: Bounds check is implicit in sp >= memory_size check above
            let value = self.memory[self.sp];
            self.sp += 1;
            Ok(value)
        }
    }

    // --- Memory Validation ---
    // Validates addresses used for word access (Li, Si) in the main `memory`
    #[inline]
    fn validate_memory_word_address(&self, addr_val: Value) -> Result<usize, VmError> {
        if addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Stack/Heap".to_string()});
        }
        let addr_usize = addr_val as usize;
        if addr_usize >= self.memory_size {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Stack/Heap".to_string()});
        }
        Ok(addr_usize)
    }

     // Validates addresses used for byte access (Lc, Sc) in the main `memory`
     // This needs careful handling as memory stores Values (i32)
     #[inline]
     fn validate_memory_byte_address(&self, addr_val: Value) -> Result<usize, VmError> {
         // For simplicity, C4 often treats char* as int* / word addresses.
         // A true byte-addressable VM is more complex.
         // Let's assume Lc/Sc operate on the low byte of the word at the given *word* address.
         self.validate_memory_word_address(addr_val)
     }

    // Validates addresses for reading bytes from the data segment
    #[inline]
    fn validate_data_segment_address(&self, addr_val: Value) -> Result<usize, VmError> {
        if addr_val < 0 {
             return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Data Segment".to_string()});
        }
        let addr_usize = addr_val as usize;
        // Important: Check against data_segment length
        if addr_usize >= self.data_segment.len() {
             return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Data Segment".to_string()});
        }
        Ok(addr_usize)
    }


    #[inline]
    fn validate_jump_target(&self, target_val: Value) -> Result<usize, VmError> {
         if target_val < 0 {
             return Err(VmError::InvalidJumpTarget(target_val));
         }
         let target_pc = target_val as usize;
         if target_pc >= self.code_size {
             return Err(VmError::InvalidJumpTarget(target_val));
         }
         Ok(target_pc)
    }

    // --- Arithmetic Helpers ---
    // (Identical to your provided code - assuming checked operations are desired)
    #[inline] fn checked_add(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_add(rhs).ok_or_else(|| VmError::ArithmeticError("Addition overflow".to_string())) }
    #[inline] fn checked_sub(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_sub(rhs).ok_or_else(|| VmError::ArithmeticError("Subtraction overflow".to_string())) }
    #[inline] fn checked_mul(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_mul(rhs).ok_or_else(|| VmError::ArithmeticError("Multiplication overflow".to_string())) }
    #[inline] fn checked_div(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Division by zero".to_string())); } lhs.checked_div(rhs).ok_or_else(|| VmError::ArithmeticError("Division overflow (INT_MIN / -1)".to_string())) }
    #[inline] fn checked_rem(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Modulo by zero".to_string())); } lhs.checked_rem(rhs).ok_or_else(|| VmError::ArithmeticError("Modulo overflow (INT_MIN % -1)".to_string())) }
    #[inline] fn checked_neg(&self, val: Value) -> Result<Value, VmError> { val.checked_neg().ok_or_else(|| VmError::ArithmeticError("Negation overflow (INT_MIN)".to_string())) }
    #[inline] fn checked_shl(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 { return Err(VmError::InvalidShiftAmount(rhs)); } let shift_amount = rhs as u32; lhs.checked_shl(shift_amount).ok_or_else(|| VmError::ArithmeticError("Shift left overflow/invalid amount".to_string())) }
    #[inline] fn checked_shr(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 { return Err(VmError::InvalidShiftAmount(rhs)); } let shift_amount = rhs as u32; lhs.checked_shr(shift_amount).ok_or_else(|| VmError::ArithmeticError("Shift right overflow/invalid amount".to_string())) }


    // --- Execution Loop ---
    pub fn run(&mut self) -> Result<Value, VmError> {
        self.running = true;
        let mut last_error: Option<VmError> = None;

        while self.running {
            // Check PC bounds before fetch_instruction
            if self.pc >= self.code_size {
                last_error = Some(VmError::PcOutOfBounds);
                self.running = false;
                break;
            }
            let current_pc = self.pc; // For error reporting

            match self.fetch_instruction() {
                Ok(instruction) => {
                    if let Err(e) = self.execute(instruction) {
                        last_error = Some(e);
                        self.running = false; // Stop on error
                    }
                }
                Err(e) => {
                    // Error fetching/decoding instruction itself
                    last_error = Some(e);
                    self.running = false;
                }
            }
        }

        // Return AX on clean exit (Exit instruction encountered), or propagate error
        match last_error {
             None => Ok(self.ax), // Normal termination via Exit instruction
             Some(e) => Err(e),  // Execution stopped due to an error
        }
    }


    /// Executes a single instruction.
    #[inline]
    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
         const PRINTF_SYSCALL_ADDR: Value = -1; // Match parser's special value

        match instruction {
            Instruction::Nop => { /* Do nothing */ }
            Instruction::Imm => { self.ax = self.fetch_operand()?; }
            Instruction::Push => self.push(self.ax)?,
            Instruction::Exit => { self.running = false; } // Signal clean exit

            // --- Arithmetic (C4 style: AX = Pop() op AX) ---
            Instruction::Add => { let op = self.pop()?; self.ax = self.checked_add(op, self.ax)?; }
            Instruction::Sub => { let op = self.pop()?; self.ax = self.checked_sub(op, self.ax)?; }
            Instruction::Mul => { let op = self.pop()?; self.ax = self.checked_mul(op, self.ax)?; }
            Instruction::Div => { let op = self.pop()?; self.ax = self.checked_div(op, self.ax)?; }
            Instruction::Mod => { let op = self.pop()?; self.ax = self.checked_rem(op, self.ax)?; }

            // --- Bitwise/Logical ---
            Instruction::Or => { let op = self.pop()?; self.ax = op | self.ax; }
            Instruction::Xor => { let op = self.pop()?; self.ax = op ^ self.ax; }
            Instruction::And => { let op = self.pop()?; self.ax = op & self.ax; }
            // --- Shifts --- (AX = Pop() << AX) (Left value = Pop, Right amount = AX)
            Instruction::Shl => { let val = self.pop()?; self.ax = self.checked_shl(val, self.ax)?; }
            Instruction::Shr => { let val = self.pop()?; self.ax = self.checked_shr(val, self.ax)?; } // Arithmetic shift for i32

            // --- Relational/Equality --- (AX = (Pop() op AX) ? 1 : 0)
            Instruction::Eq => { let op = self.pop()?; self.ax = if op == self.ax { 1 } else { 0 }; }
            Instruction::Ne => { let op = self.pop()?; self.ax = if op != self.ax { 1 } else { 0 }; }
            Instruction::Lt => { let op = self.pop()?; self.ax = if op < self.ax { 1 } else { 0 }; }
            Instruction::Gt => { let op = self.pop()?; self.ax = if op > self.ax { 1 } else { 0 }; }
            Instruction::Le => { let op = self.pop()?; self.ax = if op <= self.ax { 1 } else { 0 }; }
            Instruction::Ge => { let op = self.pop()?; self.ax = if op >= self.ax { 1 } else { 0 }; }

            // --- Unary ---
            Instruction::Neg => { self.ax = self.checked_neg(self.ax)?; }
            Instruction::Not => { self.ax = if self.ax == 0 { 1 } else { 0 }; } // Logical not

            // --- Control Flow ---
            Instruction::Jmp => { let target = self.fetch_operand()?; self.pc = self.validate_jump_target(target)?; }
            Instruction::Jz => { let target = self.fetch_operand()?; if self.ax == 0 { self.pc = self.validate_jump_target(target)?; } }
            Instruction::Jnz => { let target = self.fetch_operand()?; if self.ax != 0 { self.pc = self.validate_jump_target(target)?; } }

            // --- Function Call ---
            Instruction::Call => {
                let target_addr_val = self.fetch_operand()?; // PC now points after operand

                // --- START printf Hook ---
                if target_addr_val == PRINTF_SYSCALL_ADDR {
                    // Execute built-in printf logic
                    let str_addr_val = self.pop()?; // Pop the string address argument
                    let start_addr = self.validate_data_segment_address(str_addr_val)?; // Validate base addr

                    // Find the null terminator in the data segment
                    let mut end_addr = start_addr;
                    while end_addr < self.data_segment.len() && self.data_segment[end_addr] != 0 {
                        end_addr += 1;
                    }

                    // Check if null terminator was found within bounds
                    if end_addr == self.data_segment.len() && (start_addr == end_addr || self.data_segment[end_addr-1] != 0) {
                       // String not null-terminated or zero length starting at end
                       return Err(VmError::DataSegmentAccessError(format!("String starting at {} not null-terminated within data segment", str_addr_val)));
                    }

                    // Slice the data segment and print
                    let string_slice = &self.data_segment[start_addr..end_addr];
                    match std::str::from_utf8(string_slice) {
                        Ok(s) => {
                            // Use print! to avoid extra newline
                            print!("{}", s);
                            // C printf returns the number of characters written
                            self.ax = s.len() as Value;
                        }
                        Err(_) => {
                             return Err(VmError::DataSegmentAccessError(format!("Invalid UTF-8 sequence in data segment at address {}", str_addr_val)));
                        }
                    }
                 // --- END printf Hook ---
                } else {
                    // Regular function call
                    let target_pc = self.validate_jump_target(target_addr_val)?;
                    // Push return address (PC *after* operand) onto the main memory stack
                    self.push(self.pc as Value)?;
                    self.pc = target_pc; // Jump to function
                }
            }
            Instruction::Ent => {
                let locals_size = self.fetch_operand()?;
                if locals_size < 0 { return Err(VmError::ArithmeticError("ENT operand (locals size) cannot be negative".to_string())); }
                self.push(self.bp as Value)?;   // Push old base pointer
                self.bp = self.sp;              // Set new base pointer to current stack top (before locals)
                // Allocate space for locals by decrementing SP
                // Check for overflow before subtraction
                if self.sp < locals_size as usize { return Err(VmError::StackOverflow); }
                self.sp -= locals_size as usize;
            }
            Instruction::Adj => {
                let arg_count = self.fetch_operand()?;
                 if arg_count < 0 { return Err(VmError::ArithmeticError("ADJ operand (arg count) cannot be negative".to_string())); }
                 let new_sp = self.sp.checked_add(arg_count as usize);
                 match new_sp {
                     // Ensure SP doesn't go beyond the initial top
                     Some(sp) if sp <= self.memory_size => self.sp = sp,
                     _ => return Err(VmError::StackUnderflow), // Adjusting stack pointer went out of bounds
                 }
            }
            Instruction::Lev => {
                self.sp = self.bp;              // Deallocate locals, SP points to saved BP
                self.bp = self.pop()? as usize; // Restore old base pointer (pop BP) - cast assumes valid BP was pushed
                let return_pc_val = self.pop()?; // Pop return address
                self.pc = self.validate_jump_target(return_pc_val)?; // Jump back
            }

            // --- Memory Access (Using `memory`) ---
            Instruction::Lea => {
                let offset = self.fetch_operand()?;
                // LEA calculates address BP + offset. BP points to the location *of the saved BP*.
                // Arguments are usually at BP+N, locals at BP-N (or SP based). C4's LEA is often BP+offset.
                let base = self.bp as Value; // BP is a stack index, treat as Value for calculation
                // Use checked_add for safety
                self.ax = self.checked_add(base, offset)?;
                // Address calculated is stored in AX. Validation happens when used by LI/SI etc.
            }
            Instruction::Li => { // Load Integer from memory[AX]
                let addr = self.validate_memory_word_address(self.ax)?;
                self.ax = self.memory[addr];
            }
            Instruction::Lc => { // Load Character from memory[AX] (low byte)
                 let addr = self.validate_memory_byte_address(self.ax)?; // Validate word addr
                 // Read the word and mask to get the lower byte, zero-extend.
                 self.ax = self.memory[addr] & 0xFF;
            }
            Instruction::Si => { // Store Integer AX into memory[Pop()]
                // C4 source: *--sp = ax; ax = bp; *--sp = bp; bp = --sp; *bp = *ax;
                // C4 description often says Mem[Pop()] = AX or Mem[AX] = Pop(). Let's use Mem[Pop()] = AX
                 let addr_val = self.pop()?;
                 let addr = self.validate_memory_word_address(addr_val)?;
                 self.memory[addr] = self.ax;
            }
            Instruction::Sc => { // Store Character (low byte of AX) into memory[Pop()]
                 let addr_val = self.pop()?;
                 let addr = self.validate_memory_byte_address(addr_val)?; // Validate word addr
                 // Store only the lower byte into the target word address
                 // This overwrites parts of the existing value at memory[addr]
                 let current_val = self.memory[addr];
                 let low_byte_ax = self.ax & 0xFF;
                 self.memory[addr] = (current_val & !0xFF) | low_byte_ax; // Clear low byte, OR in new byte
            }
        } // end match instruction
        Ok(())
    }

    // --- Debugging Helpers ---
    // (Keep your existing dump_registers and dump_stack if needed)
    #[allow(dead_code)]
    pub fn dump_registers(&self) {
        println!("--- Registers ---");
        println!("  PC: {:04X} ({})", self.pc, self.pc);
        println!("  SP: {:04X} ({})", self.sp, self.sp);
        println!("  BP: {:04X} ({})", self.bp, self.bp);
        println!("  AX: {:08X} ({})", self.ax, self.ax);
        println!("-----------------");
    }

    #[allow(dead_code)]
    pub fn dump_stack(&self, count: usize) {
        println!("--- Stack Top (max {} entries) ---", count);
        println!("  SP -> 0x{:X} (Memory Index {})", self.sp, self.sp);
        let start = self.sp;
        let end = (start + count).min(self.memory_size); // Don't go past physical end
        if start >= self.memory_size {
             println!("  (Stack Empty or SP points beyond memory)");
        } else if start >= end {
             println!("  (SP points near end, showing 0 entries)");
        }
         else {
            // Print from SP upwards towards higher indices (lower on stack)
            for i in start..end {
                // Addresses increase going "up" the stack visually here
                println!("  [Mem Idx 0x{:X}] {:08X} ({})", i, self.memory[i], self.memory[i]);
            }
        }
        println!("-------------------------");
    }

     #[allow(dead_code)]
     pub fn dump_data_segment(&self) {
         println!("--- Data Segment ({} bytes) ---", self.data_segment.len());
         // Simple hex dump view
         for (i, byte) in self.data_segment.iter().enumerate() {
             if i % 16 == 0 { print!("\n  {:04X}: ", i); }
             print!("{:02X} ", byte);
         }
         println!("\n----------------------------");
     }
}