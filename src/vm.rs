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
    pc: usize,
    pub sp: usize, // Make pub for potential external inspection/debugging
    bp: usize,
    ax: Value,

    // Memory
    code: Vec<Value>,
    memory: Vec<Value>, // Combined Stack/Heap area
    data_segment: Vec<u8>,

    // State
    running: bool,
    code_size: usize,
    pub memory_size: usize, // Make pub for potential external inspection/debugging
}


impl VirtualMachine {
    /// Creates a new VM with code, data segment, and default memory size.
    pub fn new(code: Vec<Value>, data_segment: Vec<u8>) -> Self {
        Self::with_memory_size(code, data_segment, DEFAULT_MEM_SIZE)
    }

    /// Creates a new VM with specific memory size.
    pub fn with_memory_size(code: Vec<Value>, data_segment: Vec<u8>, memory_bytes: usize) -> Self {
        // Ensure memory size is appropriate for Value type
        let memory_word_size = memory_bytes / std::mem::size_of::<Value>();
        if memory_word_size == 0 {
            panic!("Memory size must be at least {} bytes", std::mem::size_of::<Value>());
        }
        let memory = vec![0; memory_word_size]; // Initialize memory with zeros
        let code_size = code.len();
        let initial_sp = memory_word_size; // SP starts just *past* the end (stack grows down)

        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp, // BP initially same as SP
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
            self.running = false; // Stop if PC goes out of bounds
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
        // Fetch value, but return OperandExpected if PC was already out of bounds
        self.fetch_value().map_err(|e| match e {
            VmError::PcOutOfBounds => VmError::OperandExpected,
            other => other,
        })
    }

    // --- Stack Operations ---
    #[inline]
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 { // Stack grows down, overflow if SP hits 0
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1; // Decrement SP *before* writing
            self.memory[self.sp] = value;
            Ok(())
        }
    }

    #[inline]
    fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= self.memory_size { // Underflow if SP is at or beyond the initial top
            Err(VmError::StackUnderflow)
        } else {
            let value = self.memory[self.sp];
            self.sp += 1; // Increment SP *after* reading
            Ok(value)
        }
    }

    // --- Memory Validation ---
    #[inline]
    fn validate_memory_word_address(&self, addr_val: Value) -> Result<usize, VmError> {
        if addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Stack/Heap".to_string()});
        }
        let addr_usize = addr_val as usize;
        if addr_usize >= self.memory_size { // Check against stack/heap size
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Stack/Heap".to_string()});
        }
        Ok(addr_usize)
    }

     #[inline]
     fn validate_memory_byte_address(&self, addr_val: Value) -> Result<usize, VmError> {
         // For Lc/Sc, we still operate on word addresses but handle bytes within that word.
         self.validate_memory_word_address(addr_val)
     }

    #[inline]
    fn validate_data_segment_address(&self, addr_val: Value) -> Result<usize, VmError> {
        if addr_val < 0 {
             return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Data Segment".to_string()});
        }
        let addr_usize = addr_val as usize;
        // Check against data_segment length
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
         // Jumps must be within the code segment bounds
         if target_pc >= self.code_size {
             return Err(VmError::InvalidJumpTarget(target_val));
         }
         Ok(target_pc)
    }

    // --- Arithmetic Helpers ---
    #[inline] fn checked_add(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_add(rhs).ok_or_else(|| VmError::ArithmeticError("Addition overflow".to_string())) }
    #[inline] fn checked_sub(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_sub(rhs).ok_or_else(|| VmError::ArithmeticError("Subtraction overflow".to_string())) }
    #[inline] fn checked_mul(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_mul(rhs).ok_or_else(|| VmError::ArithmeticError("Multiplication overflow".to_string())) }
    #[inline] fn checked_div(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Division by zero".to_string())); } lhs.checked_div(rhs).ok_or_else(|| VmError::ArithmeticError("Division overflow (INT_MIN / -1)".to_string())) }
    #[inline] fn checked_rem(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Modulo by zero".to_string())); } lhs.checked_rem(rhs).ok_or_else(|| VmError::ArithmeticError("Modulo overflow (INT_MIN % -1)".to_string())) }
    #[inline] fn checked_neg(&self, val: Value) -> Result<Value, VmError> { val.checked_neg().ok_or_else(|| VmError::ArithmeticError("Negation overflow (INT_MIN)".to_string())) }
    #[inline] fn checked_shl(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 || rhs >= 32 { return Err(VmError::InvalidShiftAmount(rhs)); } lhs.checked_shl(rhs as u32).ok_or_else(|| VmError::ArithmeticError("Shift left overflow/invalid amount".to_string())) }
    #[inline] fn checked_shr(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 || rhs >= 32 { return Err(VmError::InvalidShiftAmount(rhs)); } lhs.checked_shr(rhs as u32).ok_or_else(|| VmError::ArithmeticError("Shift right overflow/invalid amount".to_string())) }


    // --- Execution Loop ---
    pub fn run(&mut self) -> Result<Value, VmError> {
        self.running = true;
        let mut last_error: Option<VmError> = None;

        // Removed the RUN START/END debug prints
        // println!("[VM RUN START] Initial SP={:X}, BP={:X}", self.sp, self.bp);

        while self.running {
            if self.pc >= self.code_size { // Check PC bounds
                if self.pc == self.code_size { self.running = false; break; }
                else { last_error = Some(VmError::PcOutOfBounds); self.running = false; break; }
            }

            let current_pc = self.pc; // PC before fetch
            // Removed SP_before debug variable
            // let sp_before = self.sp;

            match self.fetch_instruction() {
                Ok(instruction) => {
                    // Removed the BEFORE/AFTER SP debug prints
                    // println!("[VM RUN] PC:{:04X} SP:{:X} AX:{:X} Instr:{:?}", current_pc, sp_before, self.ax, instruction);

                    let execute_result = self.execute(instruction);

                    // println!("[VM RUN] --- After PC:{:04X} SP:{:X} -> {:X}, AX:{:X}", current_pc, sp_before, self.sp, self.ax);

                    if let Err(e) = execute_result {
                        last_error = Some(e);
                        self.running = false;
                    }
                }
                Err(e) => { // Error during fetch_instruction
                    // println!("[VM RUN] Error fetching instruction at PC:{:04X}", current_pc); // Removed debug
                    last_error = Some(e);
                    self.running = false;
                }
            }
        } // end while

        // println!("[VM RUN END] Final SP={:X}, AX={:X}", self.sp, self.ax); // Removed debug
        match last_error {
             None => Ok(self.ax),
             Some(e) => Err(e),
        }
    }


    /// Executes a single instruction.
    #[inline]
    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
         const PRINTF_SYSCALL_ADDR: Value = -1; // Match parser's special value

        match instruction {
            Instruction::Nop => { /* Do nothing */ }
            Instruction::Imm => { self.ax = self.fetch_operand()?; } // AX = operand
            Instruction::Push => self.push(self.ax)?, // Push AX onto stack
            Instruction::Exit => { self.running = false; } // Signal clean exit for the run loop

            // --- Arithmetic (AX = Pop() op AX) ---
            Instruction::Add => { let op = self.pop()?; self.ax = self.checked_add(op, self.ax)?; }
            Instruction::Sub => { let op = self.pop()?; self.ax = self.checked_sub(op, self.ax)?; }
            Instruction::Mul => { let op = self.pop()?; self.ax = self.checked_mul(op, self.ax)?; }
            Instruction::Div => { let op = self.pop()?; self.ax = self.checked_div(op, self.ax)?; }
            Instruction::Mod => { let op = self.pop()?; self.ax = self.checked_rem(op, self.ax)?; }

            // --- Bitwise/Logical (AX = Pop() op AX) ---
            Instruction::Or => { let op = self.pop()?; self.ax = op | self.ax; }
            Instruction::Xor => { let op = self.pop()?; self.ax = op ^ self.ax; }
            Instruction::And => { let op = self.pop()?; self.ax = op & self.ax; }
            // --- Shifts (AX = Pop() << AX) ---
            Instruction::Shl => { let val = self.pop()?; self.ax = self.checked_shl(val, self.ax)?; }
            Instruction::Shr => { let val = self.pop()?; self.ax = self.checked_shr(val, self.ax)?; } // Arithmetic right shift for i32

            // --- Relational/Equality (AX = (Pop() op AX) ? 1 : 0) ---
            Instruction::Eq => { let op = self.pop()?; self.ax = if op == self.ax { 1 } else { 0 }; }
            Instruction::Ne => { let op = self.pop()?; self.ax = if op != self.ax { 1 } else { 0 }; }
            Instruction::Lt => { let op = self.pop()?; self.ax = if op < self.ax { 1 } else { 0 }; }
            Instruction::Gt => { let op = self.pop()?; self.ax = if op > self.ax { 1 } else { 0 }; }
            Instruction::Le => { let op = self.pop()?; self.ax = if op <= self.ax { 1 } else { 0 }; }
            Instruction::Ge => { let op = self.pop()?; self.ax = if op >= self.ax { 1 } else { 0 }; }

            // --- Unary ---
            Instruction::Neg => { self.ax = self.checked_neg(self.ax)?; } // AX = -AX
            Instruction::Not => { self.ax = if self.ax == 0 { 1 } else { 0 }; } // Logical not: AX = !AX

            // --- Control Flow ---
            Instruction::Jmp => { let target = self.fetch_operand()?; self.pc = self.validate_jump_target(target)?; }
            Instruction::Jz => { let target = self.fetch_operand()?; if self.ax == 0 { self.pc = self.validate_jump_target(target)?; } }
            Instruction::Jnz => { let target = self.fetch_operand()?; if self.ax != 0 { self.pc = self.validate_jump_target(target)?; } }

            // --- Function Call ---
            Instruction::Call => {
                let target_addr_val = self.fetch_operand()?; // Get target address or syscall ID

                // --- Check for built-in printf hook ---
                if target_addr_val == PRINTF_SYSCALL_ADDR {
                    // <<< REVERTED printf Hook Logic (Standard C Convention) >>>
                    // Assumes Parser pushed args R->L: Stack Top has fmt_addr, then arg1, arg2...

                    // <<< Removed Printf Debug prints >>>

                    // 1. Pop format string address FIRST
                    let str_addr_val = self.pop()?;
                    let start_addr = self.validate_data_segment_address(str_addr_val)?;

                    // 2. Read format string from data segment
                    let format_string_bytes: Vec<u8>;
                    { // Scope for data_segment borrow
                        let mut end_addr = start_addr;
                        while end_addr < self.data_segment.len() && self.data_segment[end_addr] != 0 { end_addr += 1; }
                        if end_addr == self.data_segment.len() && (start_addr == end_addr || self.data_segment.get(end_addr).map_or(true, |&b| b != 0)) {
                           return Err(VmError::DataSegmentAccessError(format!("String starting at {} not null-terminated within data segment", str_addr_val)));
                        }
                        format_string_bytes = self.data_segment[start_addr..end_addr].to_vec();
                    }
                    let format_str = String::from_utf8(format_string_bytes)
                        .map_err(|_| VmError::DataSegmentAccessError(format!("Invalid UTF-8 sequence in format string at address {}", str_addr_val)))?;

                    // 3. Process format string. Pop arguments as needed.
                    let mut chars_iter = format_str.chars().peekable();
                    let mut output_string = String::new();
                    let mut chars_printed_count = 0;

                    while let Some(c) = chars_iter.next() {
                        if c == '%' {
                            match chars_iter.next() {
                                Some('d') => { // Integer argument
                                    // Pop the integer argument *now*
                                    let arg_int_value = self.pop()?;
                                    let formatted_arg = arg_int_value.to_string();
                                    output_string.push_str(&formatted_arg);
                                    chars_printed_count += formatted_arg.len();
                                }
                                Some('%') => { // Literal '%'
                                    output_string.push('%');
                                    chars_printed_count += 1;
                                }
                                Some(other) => { // Unrecognized specifier
                                     output_string.push('%'); output_string.push(other); chars_printed_count += 2;
                                }
                                None => { // String ends with '%'
                                     output_string.push('%'); chars_printed_count += 1;
                                }
                            }
                        } else {
                            output_string.push(c);
                            chars_printed_count += 1;
                        }
                    } // End while loop

                    // 4. Print the composed string
                    print!("{}", output_string);

                    // 5. Set return value in AX
                    self.ax = chars_printed_count as Value;
                     // --- END REVERTED printf Hook ---

                } else {
                    // --- Regular function call ---
                    let target_pc = self.validate_jump_target(target_addr_val)?;
                    self.push(self.pc as Value)?; // Push return address
                    self.pc = target_pc; // Jump
                }
            }
            Instruction::Ent => { // Enter function: setup stack frame
                let locals_size = self.fetch_operand()?;
                // <<< Removed ENT Debug print >>>
                // println!("[VM DEBUG] ENT instruction executing with locals_size = {}", locals_size);
                if locals_size < 0 { return Err(VmError::ArithmeticError("ENT operand (locals size) cannot be negative".to_string())); }
                self.push(self.bp as Value)?;   // Push old base pointer
                self.bp = self.sp;              // Set new base pointer
                let locals_size_usize = locals_size as usize;
                if self.sp < locals_size_usize { return Err(VmError::StackOverflow); }
                self.sp -= locals_size_usize;   // Allocate space for locals
            }
            Instruction::Adj => { // Adjust stack pointer
                let arg_count = self.fetch_operand()?;
                 if arg_count < 0 { return Err(VmError::ArithmeticError("ADJ operand (arg count) cannot be negative".to_string())); }
                 let new_sp = self.sp.checked_add(arg_count as usize);
                 match new_sp {
                     Some(sp) if sp <= self.memory_size => self.sp = sp,
                     _ => return Err(VmError::StackUnderflow),
                 }
            }
            Instruction::Lev => { // Leave function
                 if self.bp >= self.memory_size { return Err(VmError::StackUnderflow); }
                 self.sp = self.bp;              // Deallocate locals
                 self.bp = self.pop()? as usize; // Restore old BP
                 let return_pc_val = self.pop()?; // Pop return address
                 self.pc = self.validate_jump_target(return_pc_val)?; // Jump back
            }

            // --- Memory Access ---
            Instruction::Lea => { // Load Effective Address
                let offset = self.fetch_operand()?;
                let base = self.bp as Value;
                self.ax = self.checked_add(base, offset)?;
            }
            Instruction::Li => { // Load Integer
                let addr = self.validate_memory_word_address(self.ax)?;
                self.ax = self.memory[addr];
            }
            Instruction::Lc => { // Load Character
                 let addr = self.validate_memory_byte_address(self.ax)?;
                 self.ax = self.memory[addr] & 0xFF;
            }
            Instruction::Si => { // Store Integer
                 let addr_val = self.pop()?;
                 let addr = self.validate_memory_word_address(addr_val)?;
                 self.memory[addr] = self.ax;
            }
            Instruction::Sc => { // Store Character
                 let addr_val = self.pop()?;
                 let addr = self.validate_memory_byte_address(addr_val)?;
                 let current_val = self.memory[addr];
                 let low_byte_ax = self.ax & 0xFF;
                 self.memory[addr] = (current_val & !0xFF) | low_byte_ax;
            }
        } // end match instruction
        Ok(())
    }

    // --- Debugging Helpers ---
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
        let end = start.saturating_add(count).min(self.memory_size);

        if start >= self.memory_size {
             println!("  (Stack Empty or SP points beyond memory)");
        } else if start >= end {
             println!("  (SP points at end or count is 0, showing 0 entries)");
        }
         else {
            for i in start..end {
                println!("  [Mem Idx 0x{:X}] {:08X} ({})", i, self.memory[i], self.memory[i]);
            }
        }
        println!("-------------------------");
    }

     #[allow(dead_code)]
     pub fn dump_data_segment(&self) {
         println!("--- Data Segment ({} bytes) ---", self.data_segment.len());
         const BYTES_PER_LINE: usize = 16;
         for (i, chunk) in self.data_segment.chunks(BYTES_PER_LINE).enumerate() {
             print!("  {:04X}: ", i * BYTES_PER_LINE); // Print address offset
             for byte in chunk { print!("{:02X} ", byte); }
             if chunk.len() < BYTES_PER_LINE { for _ in 0..(BYTES_PER_LINE - chunk.len()) { print!("   "); } }
             print!(" |");
             for byte in chunk { print!("{}", if *byte >= 32 && *byte <= 126 { *byte as char } else { '.' }); }
             println!("|");
         }
         if self.data_segment.is_empty() { println!("  (Empty)"); }
         println!("----------------------------");
     }
}