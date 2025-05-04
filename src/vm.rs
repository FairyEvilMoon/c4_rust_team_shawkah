// src/vm.rs

use std::convert::TryFrom;
// Required for libc calls and C string handling
use std::ffi::CString; // Removed unused CStr
// Removed: use std::os::unix::ffi::OsStrExt;

// --- Types and Constants ---

pub type Value = i32; // VM uses 32-bit integers
pub const VALUE_SIZE_BYTES: usize = std::mem::size_of::<Value>();
pub const DEFAULT_MEM_SIZE_WORDS: usize = 256 * 1024; // 256k words = 1MB for i32
pub const DEFAULT_MEM_SIZE_BYTES: usize = DEFAULT_MEM_SIZE_WORDS * VALUE_SIZE_BYTES;
// Stack grows downwards from high memory in this model
pub const STACK_TOP_ADDR_WORDS: usize = DEFAULT_MEM_SIZE_WORDS; // Initial SP value

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
    // Memory Access (Now generally using byte addresses)
    Li = 16, Lc = 17, Si = 18, Sc = 19, Lea = 20, // Load Integer, Load Char, Store Int, Store Char, Load Effective Address
    // Bitwise/Logical (Pop() op AX) -> AX = Op1 op Op2
    Or  = 21, Xor = 22, And = 23, Shl = 24, Shr = 25,
    // Relational (Pop() op AX) -> AX = (Op1 op Op2) ? 1 : 0
    Eq = 26, Ne = 27, Lt = 28, Gt = 29, Le = 30, Ge = 31,
    // Unary
    Neg = 32, Not = 33, // Negate, Logical NOT (!=0 -> 0, ==0 -> 1)

    // C4 Syscalls/Library Functions mapped to Opcodes
    Open = 34, Read = 35, Clos = 36, Prtf = 37, Malc = 38, Free = 39, Mset = 40, Mcmp = 41,Pow = 42,
    // NOTE: C4's EXIT is mapped to opcode 42, but we use Instruction::Exit (opcode 3) for VM control.
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
            // Syscalls
            34 => Ok(Instruction::Open), 35 => Ok(Instruction::Read), 36 => Ok(Instruction::Clos),
            37 => Ok(Instruction::Prtf), 38 => Ok(Instruction::Malc), 39 => Ok(Instruction::Free),
            40 => Ok(Instruction::Mset), 41 => Ok(Instruction::Mcmp), 42 => Ok(Instruction::Pow),
            _ => Err(VmError::InvalidInstruction(value)),
        }
    }
}


// --- Errors ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    InvalidInstruction(Value),
    PcOutOfBounds,
    StackOverflow, // Collision between stack and heap
    StackUnderflow,
    HeapOverflow, // For simulated malloc (or stack collision)
    OperandExpected,
    ArithmeticError(String),
    InvalidJumpTarget(Value),
    MemoryAccessOutOfBounds { address: Value, size: usize, op_name: String }, // Byte address, size in bytes
    UnalignedMemoryAccess { address: Value, op_name: String }, // For instructions that might require alignment
    InvalidShiftAmount(Value),
    // DataSegmentAccessError(String), // Removed, data segment is part of unified memory
    InvalidSyscall(Value),
    InvalidFileDescriptor(Value),
    NullPointerAccess(String),
    CStringError(String), // Errors reading C strings from memory
    IoError(String), // Wrapper for underlying IO errors
    InitializationError(String),
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         match self {
            VmError::InvalidInstruction(v) => write!(f, "Invalid instruction opcode: {}", v),
            VmError::PcOutOfBounds => write!(f, "Program counter out of code bounds"),
            VmError::StackOverflow => write!(f, "Stack overflow (collided with heap or memory start)"),
            VmError::StackUnderflow => write!(f, "Stack underflow"),
            VmError::HeapOverflow => write!(f, "Heap overflow (allocation failed or collided with stack)"),
            VmError::OperandExpected => write!(f, "Operand expected, but reached end of code"),
            VmError::ArithmeticError(s) => write!(f, "Arithmetic error: {}", s),
            VmError::InvalidJumpTarget(v) => write!(f, "Invalid jump target address in code: {}", v),
            VmError::MemoryAccessOutOfBounds{ address, size, op_name } => write!(f, "Memory access out of bounds during '{}': address=0x{:X} ({}), size={} bytes", op_name, address, address, size),
            VmError::UnalignedMemoryAccess { address, op_name } => write!(f, "Unaligned memory access during '{}' at byte address 0x{:X} ({})", op_name, address, address),
            VmError::InvalidShiftAmount(v) => write!(f, "Invalid shift amount: {}", v),
            // VmError::DataSegmentAccessError(s) => write!(f, "Data Segment access error: {}", s), // Removed
            VmError::InvalidSyscall(v) => write!(f, "Invalid syscall number: {}", v),
            VmError::InvalidFileDescriptor(v) => write!(f, "Invalid file descriptor: {}", v),
            VmError::NullPointerAccess(s) => write!(f, "Null pointer access during {}", s),
            VmError::CStringError(s) => write!(f, "C String error: {}", s),
            VmError::IoError(s) => write!(f, "I/O Error: {}", s),
            VmError::InitializationError(s) => write!(f, "VM Initialization Error: {}", s),
        }
    }
}
impl std::error::Error for VmError {}


// --- Virtual Machine ---
#[derive(Debug)]
pub struct VirtualMachine {
    // Registers
    pc: usize, // Program Counter (index into `code` vector)
    pub sp: usize, // Stack pointer (WORD index into `memory`, starts high, grows down)
    bp: usize, // Base pointer (WORD index into `memory`)
    ax: Value, // Accumulator

    // Memory
    code: Vec<Value>,
    // *** Unified Memory Area ***
    // Holds Data Segment, BSS, Heap (growing up), and Stack (growing down)
    memory: Vec<Value>,        // Main memory, accessed as words or bytes
    heap_ptr: usize,           // Heap pointer (WORD index, starts after data/bss, grows up)

    // State
    running: bool,
    code_size: usize,
    memory_size_words: usize,  // Total size of `memory` in words
    memory_size_bytes: usize,  // Total size of `memory` in bytes
}


impl VirtualMachine {
    /// Creates a new VM with code, initial data, and default memory size.
    pub fn new(code: Vec<Value>, initial_data: Vec<u8>) -> Result<Self, VmError> {
        Self::with_memory_size(code, initial_data, DEFAULT_MEM_SIZE_WORDS)
    }

    /// Creates a new VM with specific memory size (in words).
    /// The `initial_data` is copied to the beginning of the unified `memory`.
    pub fn with_memory_size(code: Vec<Value>, initial_data: Vec<u8>, memory_words: usize) -> Result<Self, VmError> {
        if memory_words == 0 {
            return Err(VmError::InitializationError("Memory size must be at least 1 word".to_string()));
        }
        let memory_size_bytes = memory_words.checked_mul(VALUE_SIZE_BYTES)
            .ok_or_else(|| VmError::InitializationError("Memory size in bytes calculation overflowed".to_string()))?;

        if initial_data.len() > memory_size_bytes {
             return Err(VmError::InitializationError(format!(
                 "Initial data size ({} bytes) exceeds total memory size ({} bytes)",
                 initial_data.len(), memory_size_bytes
             )));
        }

        let mut memory = vec![0 as Value; memory_words]; // Initialize memory with zeros

        // Copy initial data into the beginning of the memory vector using byte access
        if !initial_data.is_empty() {
            // Get a mutable byte slice of the beginning of `memory`
            let memory_bytes: &mut [u8] = unsafe {
                std::slice::from_raw_parts_mut(
                    memory.as_mut_ptr() as *mut u8,
                    memory_size_bytes // Use the total byte capacity
                )
            };
            // Copy data into the slice
            memory_bytes[..initial_data.len()].copy_from_slice(&initial_data);
        }

        // Calculate initial heap pointer (word index) to start *after* the initial data/bss
        // Round up data size to the nearest word boundary
        let data_words = (initial_data.len() + VALUE_SIZE_BYTES - 1) / VALUE_SIZE_BYTES;
        let initial_heap_ptr = data_words; // Heap starts immediately after the data words

        let code_size = code.len();
        let initial_sp = memory_words; // SP starts just *past* the end (stack grows down)

        Ok(VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp, // BP initially same as SP
            ax: 0,
            code,
            memory,
            heap_ptr: initial_heap_ptr, // Heap starts after copied data
            running: false,
            code_size,
            memory_size_words: memory_words,
            memory_size_bytes: memory_size_bytes,
        })
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

    // --- Stack Operations ---
    #[inline]
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        // Stack grows down (sp decreases). Check collision with heap_ptr (heap grows up).
        // `sp` points to the *next available* slot. `heap_ptr` points *past* the current heap.
        // Collision occurs if the *new* sp would be <= heap_ptr.
        if self.sp == 0 || (self.sp - 1) < self.heap_ptr {
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1; // Decrement SP *before* writing
            // Bounds check (redundant if sp > 0 check is sufficient, but safer)
            if self.sp >= self.memory_size_words {
                 Err(VmError::StackOverflow) // Should not happen if initial checks work
            } else {
                self.memory[self.sp] = value;
                Ok(())
            }
        }
    }

    #[inline]
    fn pop(&mut self) -> Result<Value, VmError> {
        // Stack grows down, so pop increases SP.
        // Underflow if SP is at or beyond the initial top address.
        if self.sp >= self.memory_size_words {
            Err(VmError::StackUnderflow)
        } else {
            let value = self.memory[self.sp];
            self.sp += 1; // Increment SP *after* reading
            Ok(value)
        }
    }

    // --- Memory Validation and Access (Unified Memory, Byte Addressing) ---

    /// Validates a byte address and size for memory access.
    /// Returns the starting byte index if valid.
    #[inline]
    fn validate_byte_access(&self, byte_addr_val: Value, access_size_bytes: usize, op_name: &str) -> Result<usize, VmError> {
        if byte_addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, size: access_size_bytes, op_name: op_name.to_string() });
        }
        if access_size_bytes == 0 {
            // Allow 0-byte access at valid address or just past the end
             let start_byte = byte_addr_val as usize;
             if start_byte <= self.memory_size_bytes {
                 return Ok(start_byte)
             } else {
                 return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, size: 0, op_name: op_name.to_string() });
             }
        }

        let start_byte = byte_addr_val as usize;
        let end_byte = start_byte.checked_add(access_size_bytes).ok_or_else(|| VmError::ArithmeticError(format!("Address overflow calculating end boundary for '{}'", op_name)))?;

        // Check if the entire range [start_byte, end_byte) is within bounds
        if end_byte > self.memory_size_bytes {
            // Report the address that caused the violation (could be start or partway through)
            let offending_addr = if start_byte >= self.memory_size_bytes { start_byte } else { self.memory_size_bytes } as Value;
            return Err(VmError::MemoryAccessOutOfBounds { address: offending_addr, size: access_size_bytes, op_name: op_name.to_string() });
        }
        Ok(start_byte)
    }

    /// Gets a single byte from unified memory using a byte address.
    #[inline]
    fn get_mem_byte(&self, byte_addr_val: Value, op_name: &str) -> Result<u8, VmError> {
        let byte_addr = self.validate_byte_access(byte_addr_val, 1, op_name)?;

        let word_index = byte_addr / VALUE_SIZE_BYTES;
        let byte_offset = byte_addr % VALUE_SIZE_BYTES;

        // Redundant check, validate_byte_access should cover this
        // if word_index >= self.memory_size_words { ... }

        let word = self.memory[word_index];
        // Assuming little-endian byte order within the Value (i32)
        Ok((word >> (byte_offset * 8)) as u8)
    }

    /// Sets a single byte in unified memory using a byte address.
    #[inline]
    fn set_mem_byte(&mut self, byte_addr_val: Value, byte_val: u8, op_name: &str) -> Result<(), VmError> {
        let byte_addr = self.validate_byte_access(byte_addr_val, 1, op_name)?;

        let word_index = byte_addr / VALUE_SIZE_BYTES;
        let byte_offset = byte_addr % VALUE_SIZE_BYTES;

        // Redundant check, validate_byte_access should cover this
        // if word_index >= self.memory_size_words { ... }

        // Read-Modify-Write the containing word
        let word = &mut self.memory[word_index];
        let mask = !(0xFF << (byte_offset * 8)); // Mask to clear the target byte
        *word = (*word & mask) | ((byte_val as Value) << (byte_offset * 8)); // Set the new byte
        Ok(())
    }

     /// Helper to get a mutable byte slice of the VM's unified memory.
     /// Takes byte address and byte count. Validates bounds.
    fn get_memory_slice_mut(&mut self, byte_addr: Value, count: Value, op_name: &str) -> Result<&mut [u8], VmError> {
        if count < 0 {
             return Err(VmError::MemoryAccessOutOfBounds { address: count, size: 0, op_name: format!("{} slice (negative count)", op_name) });
        }
        let num_bytes = count as usize;
        let start_byte = self.validate_byte_access(byte_addr, num_bytes, op_name)?;
        let end_byte = start_byte + num_bytes; // Already validated by validate_byte_access

        // Get a mutable slice of the underlying Vec<Value> as bytes
        let memory_bytes: &mut [u8] = unsafe {
            std::slice::from_raw_parts_mut(
                self.memory.as_mut_ptr() as *mut u8,
                self.memory_size_bytes // Use the actual total byte capacity
            )
        };

        // Slice the byte slice - this is safe because we validated bounds above
        Ok(&mut memory_bytes[start_byte..end_byte])
    }

    // Removed: validate_data_segment_address

    #[inline]
    fn validate_jump_target(&self, target_val: Value) -> Result<usize, VmError> {
         if target_val < 0 { return Err(VmError::InvalidJumpTarget(target_val)); }
         let target_pc = target_val as usize;
         // Allow jump *to* the end (pc == code_size), which signals program end
         if target_pc > self.code_size { return Err(VmError::InvalidJumpTarget(target_val)); }
         Ok(target_pc)
    }

    /// Reads a null-terminated C string from unified memory.
    /// `addr_val` is the starting byte address.
    /// `max_len` prevents reading excessive amounts of memory.
    fn read_c_string(&self, addr_val: Value, max_len: usize, op_name: &str) -> Result<CString, VmError> {
        if addr_val == 0 { // Check NULL pointer
            return Err(VmError::NullPointerAccess(format!("reading C string for {}", op_name)));
        }
        // Initial validation just for the starting byte
        let _ = self.validate_byte_access(addr_val, 1, &format!("C string start for {}", op_name))?;

        let mut bytes = Vec::new();
        let mut current_addr_val = addr_val;

        loop {
            // We need to check each byte access individually inside the loop
            let byte = self.get_mem_byte(current_addr_val, &format!("C string read for {}", op_name))?;

            if byte == 0 { break; } // Null terminator found
            bytes.push(byte);

            // Check length limit
            if bytes.len() >= max_len {
                 return Err(VmError::CStringError(format!("String for {} starting at 0x{:X} exceeded max length {}", op_name, addr_val, max_len)));
            }

            // Increment byte address, checking for potential overflow
            current_addr_val = current_addr_val.checked_add(1).ok_or_else(|| VmError::ArithmeticError(format!("Address overflow reading C String for {} near 0x{:X}", op_name, current_addr_val)))?;
        }

        // CString::new checks for interior null bytes, which shouldn't happen if our loop is correct,
        // but it's good practice to handle the Result.
        CString::new(bytes).map_err(|e| VmError::CStringError(format!("Invalid byte sequence found in C string for {} at 0x{:X}: {}", op_name, addr_val, e)))
    }


    // --- Arithmetic Helpers ---
    // (checked operations remain the same as before)
    #[inline] fn checked_add(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_add(rhs).ok_or_else(|| VmError::ArithmeticError("Addition overflow".to_string())) }
    #[inline] fn checked_sub(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_sub(rhs).ok_or_else(|| VmError::ArithmeticError("Subtraction overflow".to_string())) }
    #[inline] fn checked_mul(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { lhs.checked_mul(rhs).ok_or_else(|| VmError::ArithmeticError("Multiplication overflow".to_string())) }
    #[inline] fn checked_div(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Division by zero".to_string())); } lhs.checked_div(rhs).ok_or_else(|| VmError::ArithmeticError("Division overflow (INT_MIN / -1)".to_string())) }
    #[inline] fn checked_rem(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs == 0 { return Err(VmError::ArithmeticError("Modulo by zero".to_string())); } lhs.checked_rem(rhs).ok_or_else(|| VmError::ArithmeticError("Modulo overflow (INT_MIN % -1)".to_string())) }
    #[inline] fn checked_neg(&self, val: Value) -> Result<Value, VmError> { val.checked_neg().ok_or_else(|| VmError::ArithmeticError("Negation overflow (INT_MIN)".to_string())) }
    #[inline] fn checked_shl(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 || rhs >= 32 { return Err(VmError::InvalidShiftAmount(rhs)); } lhs.checked_shl(rhs as u32).ok_or_else(|| VmError::ArithmeticError(format!("Shift left overflow/invalid: {} << {}", lhs, rhs))) }
    #[inline] fn checked_shr(&self, lhs: Value, rhs: Value) -> Result<Value, VmError> { if rhs < 0 || rhs >= 32 { return Err(VmError::InvalidShiftAmount(rhs)); } lhs.checked_shr(rhs as u32).ok_or_else(|| VmError::ArithmeticError(format!("Shift right overflow/invalid: {} >> {}", lhs, rhs))) }


    // --- Execution Loop ---
    pub fn run(&mut self) -> Result<Value, VmError> {
        if self.running {
             return Err(VmError::IoError("VM already running".to_string())); // Or a specific state error
        }
        self.running = true;
        let mut final_result: Result<Value, VmError> = Ok(self.ax); // Default result

        while self.running {
            if self.pc >= self.code_size {
                 if self.pc == self.code_size {
                     self.running = false;
                     final_result = Ok(self.ax);
                 } else {
                     final_result = Err(VmError::PcOutOfBounds);
                     self.running = false;
                 }
                 break;
             }

            let current_pc = self.pc;
            match self.fetch_instruction().and_then(|instr| self.execute(instr)) {
                Ok(()) => {
                    if !self.running { // Check if Instruction::Exit was executed
                       final_result = Ok(self.ax);
                    }
                }
                Err(e) => {
                    eprintln!("VM Error at PC=0x{:X}: {}", current_pc, e);
                    self.dump_registers();
                    self.dump_stack(10);
                    // self.dump_memory_section(0, 32).ok(); // Optional: dump start of memory
                    final_result = Err(e);
                    self.running = false;
                }
            }
        }

        self.running = false;
        final_result
    }


    /// Executes a single instruction.
    #[inline]
    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
        match instruction {
            Instruction::Nop => { }
            Instruction::Imm => { self.ax = self.fetch_operand()?; }
            Instruction::Push => self.push(self.ax)?,
            Instruction::Exit => {
                self.running = false; // The run loop will capture AX
            }

            // --- Arithmetic / Bitwise / Relational / Unary --- (No changes needed here)
            Instruction::Add => { let op = self.pop()?; self.ax = self.checked_add(op, self.ax)?; }
            Instruction::Sub => { let op = self.pop()?; self.ax = self.checked_sub(op, self.ax)?; }
            Instruction::Mul => { let op = self.pop()?; self.ax = self.checked_mul(op, self.ax)?; }
            Instruction::Div => { let op = self.pop()?; self.ax = self.checked_div(op, self.ax)?; }
            Instruction::Mod => { let op = self.pop()?; self.ax = self.checked_rem(op, self.ax)?; }
            Instruction::Or => { let op = self.pop()?; self.ax = op | self.ax; }
            Instruction::Xor => { let op = self.pop()?; self.ax = op ^ self.ax; }
            Instruction::And => { let op = self.pop()?; self.ax = op & self.ax; }
            Instruction::Shl => { let shift_amount = self.pop()?; self.ax = self.checked_shl(self.ax, shift_amount)?; }
            Instruction::Shr => { let shift_amount = self.pop()?; self.ax = self.checked_shr(self.ax, shift_amount)?; }
            Instruction::Eq => { let op = self.pop()?; self.ax = if op == self.ax { 1 } else { 0 }; }
            Instruction::Ne => { let op = self.pop()?; self.ax = if op != self.ax { 1 } else { 0 }; }
            Instruction::Lt => { let op = self.pop()?; self.ax = if op < self.ax { 1 } else { 0 }; }
            Instruction::Gt => { let op = self.pop()?; self.ax = if op > self.ax { 1 } else { 0 }; }
            Instruction::Le => { let op = self.pop()?; self.ax = if op <= self.ax { 1 } else { 0 }; }
            Instruction::Ge => { let op = self.pop()?; self.ax = if op >= self.ax { 1 } else { 0 }; }
            Instruction::Neg => { self.ax = self.checked_neg(self.ax)?; }
            Instruction::Not => { self.ax = if self.ax == 0 { 1 } else { 0 }; }

            // --- Control Flow --- (No changes needed here)
            Instruction::Jmp => { let target = self.fetch_operand()?; self.pc = self.validate_jump_target(target)?; }
            Instruction::Jz => { let target = self.fetch_operand()?; if self.ax == 0 { self.pc = self.validate_jump_target(target)?; } }
            Instruction::Jnz => { let target = self.fetch_operand()?; if self.ax != 0 { self.pc = self.validate_jump_target(target)?; } }

            // --- Function Call ---
            Instruction::Call => {
                let target_addr_val = self.fetch_operand()?;
                let target_pc = self.validate_jump_target(target_addr_val)?;
                 let return_pc = self.pc as Value; // PC points to the *next* instruction
                 self.push(return_pc)?;
                 self.pc = target_pc;
            }
            Instruction::Ent => { // Enter function, allocate local stack frame
                let locals_size_words_val = self.fetch_operand()?;
                if locals_size_words_val < 0 { return Err(VmError::ArithmeticError(format!("ENT operand must be non-negative: {}", locals_size_words_val))); }
                let locals_size_words = locals_size_words_val as usize;

                self.push(self.bp as Value)?; // Push old BP
                self.bp = self.sp;            // New BP is current SP (top of frame before locals)

                // Allocate space for locals by decrementing SP. Check against heap_ptr.
                let next_sp = self.sp.checked_sub(locals_size_words);
                match next_sp {
                     // Check if new SP is valid *and* >= heap_ptr
                     Some(sp) if sp >= self.heap_ptr => {
                         self.sp = sp;
                         // Optional: Zero out locals? C doesn't guarantee this.
                         // for i in 0..locals_size_words { self.memory[self.sp + i] = 0; }
                     }
                     _ => { // Stack overflow (hit heap or wrapped below 0)
                         // Don't need to restore BP, VM will stop
                         return Err(VmError::StackOverflow);
                     }
                 }
            }
            Instruction::Adj => { // Adjust stack pointer after call (remove arguments)
                let arg_count_val = self.fetch_operand()?;
                 if arg_count_val < 0 { return Err(VmError::ArithmeticError(format!("ADJ operand must be non-negative: {}", arg_count_val))); }
                 let arg_count_words = arg_count_val as usize;

                 // Adjust SP upwards to remove arguments pushed by caller
                 let new_sp = self.sp.checked_add(arg_count_words);
                 match new_sp {
                     // Check if new SP is valid (doesn't go beyond original memory top)
                     Some(sp) if sp <= self.memory_size_words => self.sp = sp,
                     _ => return Err(VmError::StackUnderflow), // Removing too many args or overflow
                 }
            }
            Instruction::Lev => { // Leave function, restore stack frame
                 // Check if BP is valid before using it (should point within allocated stack)
                 if self.bp > self.memory_size_words || self.bp < self.sp {
                     // BP seems invalid or points outside the current stack bounds
                     return Err(VmError::StackUnderflow); // Or a specific BP error
                 }
                 // Deallocate locals: SP = BP (BP points *before* locals/old BP)
                 self.sp = self.bp;

                 // Restore caller's BP (pop from stack)
                 let old_bp_val = self.pop()?;
                 // Basic validation for popped BP value
                 if old_bp_val < 0 || old_bp_val as usize > self.memory_size_words {
                    return Err(VmError::StackUnderflow); // Popped invalid BP
                 }
                 self.bp = old_bp_val as usize;

                 // Pop return address
                 let return_pc_val = self.pop()?;
                 self.pc = self.validate_jump_target(return_pc_val)?; // Jump back
            }

            // --- Memory Access (Using Byte Addresses) ---
            Instruction::Lea => { // Load Effective Address: Calculates address -> AX
                let offset_words = self.fetch_operand()?; // Assume offset is in words relative to BP

                // BP is a word index. Calculate target word index.
                 let base_word_addr = self.bp as i64; // Use i64 for intermediate calculation
                 let target_word_addr_i64 = base_word_addr.checked_add(offset_words as i64).ok_or_else(|| VmError::ArithmeticError("LEA word address calculation overflow".to_string()))?;

                 // Convert target word address to target byte address
                 let target_byte_addr_i64 = target_word_addr_i64.checked_mul(VALUE_SIZE_BYTES as i64).ok_or_else(|| VmError::ArithmeticError("LEA byte address calculation overflow".to_string()))?;

                 // Check if the result fits in Value (i32)
                 if target_byte_addr_i64 < (Value::MIN as i64) || target_byte_addr_i64 > (Value::MAX as i64) {
                     return Err(VmError::ArithmeticError("LEA result out of Value range".to_string()));
                 }

                 // LEA calculates the address; it doesn't access memory itself.
                 // The calculated address might be out of bounds, but that's okay here.
                 // Subsequent LI/SI/LC/SC will fail if the address is invalid.
                 self.ax = target_byte_addr_i64 as Value;
            }
            Instruction::Li => { // Load Integer (from BYTE address in AX) -> AX
                let byte_addr_val = self.ax;
                let start_byte = self.validate_byte_access(byte_addr_val, VALUE_SIZE_BYTES, "LI")?;

                // Read VALUE_SIZE_BYTES bytes starting from start_byte
                let mut bytes = [0u8; VALUE_SIZE_BYTES];
                for i in 0..VALUE_SIZE_BYTES {
                    // Reading bytes individually handles potential unaligned access across words
                    bytes[i] = self.get_mem_byte((byte_addr_val as i64 + i as i64) as Value, "LI byte read")?;
                }
                self.ax = Value::from_le_bytes(bytes); // Reconstruct from little-endian bytes
            }
            Instruction::Lc => { // Load Character (from BYTE address in AX) -> AX
                let byte_addr_val = self.ax;
                let byte_val = self.get_mem_byte(byte_addr_val, "LC")?;
                self.ax = byte_val as Value; // Zero-extend char to Value
            }
            Instruction::Si => { // Store Integer (AX into BYTE address from stack)
                 let byte_addr_val = self.pop()?;
                 let start_byte = self.validate_byte_access(byte_addr_val, VALUE_SIZE_BYTES, "SI")?;

                 let bytes_to_write = self.ax.to_le_bytes();
                 for i in 0..VALUE_SIZE_BYTES {
                     // Writing bytes individually handles potential unaligned access across words
                     self.set_mem_byte((byte_addr_val as i64 + i as i64) as Value, bytes_to_write[i], "SI byte write")?;
                 }
            }
            Instruction::Sc => { // Store Character (AX low byte into BYTE address from stack)
                 let byte_addr_val = self.pop()?;
                 let byte_to_store = (self.ax & 0xFF) as u8;
                 self.set_mem_byte(byte_addr_val, byte_to_store, "SC")?;
            }

            // --- Syscalls --- (Arguments assumed pushed R->L: [..., argN, ..., arg1] <- SP)
            Instruction::Open => { // open(pathname_addr, flags) -> fd ; Stack: [..., pathname_addr, flags]
                let flags = self.pop()?;
                let pathname_addr = self.pop()?;
                let pathname_c = self.read_c_string(pathname_addr, 1024, "OPEN pathname")?;
                self.ax = unsafe { libc::open(pathname_c.as_ptr(), flags as libc::c_int) };
                // Optional: Check self.ax == -1 and map to VmError::IoError
            }
            Instruction::Read => { // read(fd, buf_addr, count) -> bytes_read ; Stack: [..., fd, buf_addr, count]
                let count_val = self.pop()?;
                let buf_addr = self.pop()?;
                let fd = self.pop()?;

                if fd < 0 { return Err(VmError::InvalidFileDescriptor(fd)); }
                if buf_addr == 0 { return Err(VmError::NullPointerAccess("READ buffer".to_string())); }
                 // Allow count == 0,libc::read handles it (returns 0)

                // get_memory_slice_mut validates buf_addr and count
                let buf_slice = self.get_memory_slice_mut(buf_addr, count_val, "READ")?;

                // Handle negative count after getting slice (which requires non-negative count)
                if count_val < 0 {
                     // POSIX read with negative count is undefined behavior/error.
                     // Set errno? Return -1? Let's return -1.
                     self.ax = -1;
                     // Maybe set an OS error number via a helper?
                } else if count_val == 0 {
                    self.ax = 0; // Read 0 bytes
                }
                else {
                    let bytes_read = unsafe {
                        libc::read(fd, buf_slice.as_mut_ptr() as *mut libc::c_void, buf_slice.len().try_into().unwrap()) // Use slice len
                    };
                    self.ax = bytes_read as Value; // bytes_read is isize, cast to i32
                    // Optional: Check self.ax == -1 and map to VmError::IoError
                }
            }
            Instruction::Clos => { // close(fd) -> 0 or -1 ; Stack: [..., fd]
                let fd = self.pop()?;
                if fd < 0 { return Err(VmError::InvalidFileDescriptor(fd)); }
                self.ax = unsafe { libc::close(fd) };
                 // Optional: Check self.ax == -1 and map to VmError::IoError
            }
            Instruction::Prtf => { // printf(format_addr, ...) -> count
                self.handle_printf()?; // Uses unified memory via read_c_string
            }
            Instruction::Malc => { // malloc(size_bytes) -> ptr ; Stack: [..., size_bytes]
                let size_bytes_val = self.pop()?;
                if size_bytes_val <= 0 {
                    self.ax = 0; // malloc(0) or malloc(<0) returns NULL (or implementation defined ptr)
                } else {
                    let size_bytes_usize = size_bytes_val as usize;
                    // Align size up to word boundary for simplicity, although C malloc has stricter alignment guarantees.
                    let required_bytes_aligned = (size_bytes_usize + VALUE_SIZE_BYTES - 1) & !(VALUE_SIZE_BYTES - 1);
                    let required_words = required_bytes_aligned / VALUE_SIZE_BYTES;

                    let next_heap_ptr = self.heap_ptr.checked_add(required_words);

                    match next_heap_ptr {
                         // Check collision with stack pointer (heap grows up, stack grows down)
                         // Collision if next_heap_ptr would reach or exceed current sp
                         Some(next_hp) if next_hp <= self.sp => {
                             let alloc_start_word = self.heap_ptr;
                             let alloc_addr_bytes = alloc_start_word * VALUE_SIZE_BYTES;
                             self.ax = alloc_addr_bytes as Value; // Return byte address of allocation start
                             self.heap_ptr = next_hp; // Update heap pointer *after* calculating return address

                             // Optional: Zero out allocated memory (like calloc)? C malloc doesn't.
                         }
                         _ => { // Heap overflow (collision or usize overflow)
                            self.ax = 0; // C returns NULL (0) on failure
                            // Optionally return VmError::HeapOverflow, but C programs expect NULL
                            // return Err(VmError::HeapOverflow);
                         }
                    }
                }
            }
            Instruction::Free => { // free(ptr) -> void ; Stack: [..., ptr]
                let _ptr = self.pop()?; // Pop pointer from stack
                // Our simple bump allocator doesn't support freeing individual blocks. This is a No-Op.
                self.ax = 0; // free has no return value, set AX to 0.
            }
            Instruction::Mset => { // memset(dest_addr, char_val, count) -> dest_addr ; Stack: [..., dest_addr, char_val, count]
                 let count_val = self.pop()?;
                 let char_val = self.pop()?; // Value containing the byte in its lower 8 bits
                 let dest_addr = self.pop()?;

                 if dest_addr == 0 { return Err(VmError::NullPointerAccess("MSET destination".to_string())); }
                 // Allow count == 0, should do nothing and return dest_addr

                 // get_memory_slice_mut handles validation of dest_addr and count
                 let dest_slice = self.get_memory_slice_mut(dest_addr, count_val, "MSET")?;

                 // Check count after slice aquisition (get_memory_slice_mut checks >= 0)
                 if count_val > 0 {
                    let byte_to_set = (char_val & 0xFF) as u8;
                    dest_slice.fill(byte_to_set);
                 }
                 self.ax = dest_addr; // memset returns the destination pointer
            }
            Instruction::Mcmp => { // memcmp(addr1, addr2, count) -> comparison_result ; Stack: [..., addr1, addr2, count]
                 let count_val = self.pop()?;
                 let addr2 = self.pop()?;
                 let addr1 = self.pop()?;

                 if addr1 == 0 { return Err(VmError::NullPointerAccess("MCMP address 1".to_string())); }
                 if addr2 == 0 { return Err(VmError::NullPointerAccess("MCMP address 2".to_string())); }
                 // Allow count == 0, should return 0

                if count_val < 0 {
                    // Undefined behavior in C? Let's return 0, similar to count == 0.
                    self.ax = 0;
                } else if count_val == 0 {
                    self.ax = 0;
                }
                 else {
                    let count = count_val as usize;
                    let mut result: Value = 0;

                    // Avoid slice borrowing issues by reading bytes directly
                    for i in 0..count {
                        // Calculate addresses for each byte carefully, checking for overflow
                        let current_addr1 = addr1.checked_add(i as Value).ok_or_else(|| VmError::ArithmeticError("MCMP address 1 overflow".to_string()))?;
                        let current_addr2 = addr2.checked_add(i as Value).ok_or_else(|| VmError::ArithmeticError("MCMP address 2 overflow".to_string()))?;

                        // Use get_mem_byte which handles bounds checking per byte
                        let byte1 = self.get_mem_byte(current_addr1, "MCMP byte1")?;
                        let byte2 = self.get_mem_byte(current_addr2, "MCMP byte2")?;

                        if byte1 != byte2 {
                            // C memcmp returns difference between the first differing *unsigned chars*
                            result = (byte1 as Value) - (byte2 as Value);
                            break;
                        }
                    }
                    self.ax = result; // 0 if equal, <0 if s1<s2, >0 if s1>s2
                 }
            }

            Instruction::Pow => {
                // Stack: [base_val (lhs)], AX: exponent_val (rhs)
                let exponent = self.ax;
                let base = self.pop()?; // Pop base from stack
    
                // Integer exponentiation: Use checked_pow
                // checked_pow requires a u32 exponent. Handle errors.
                if exponent < 0 {
                    return Err(VmError::ArithmeticError(format!(
                        "Negative exponent ({}) not supported for integer power", exponent
                    )));
                }
                 // Check if exponent fits in u32
                 match u32::try_from(exponent) {
                     Ok(exp_u32) => {
                         match base.checked_pow(exp_u32) {
                            Some(result) => self.ax = result,
                            None => return Err(VmError::ArithmeticError(format!(
                                "Exponentiation overflow: {} ** {}", base, exponent
                            ))),
                        }
                     }
                     Err(_) => {
                         // Exponent too large for u32
                          return Err(VmError::ArithmeticError(format!(
                             "Exponent ({}) too large for integer power", exponent
                         )));
                     }
                 }
            }

        } // end match instruction
        Ok(())
    }

    /// Handles the printf logic (separated for clarity)
    /// Assumes arguments are on the stack [..., format_addr, argN, ..., arg1]
    /// Pops arguments as needed. Uses unified memory via read_c_string.
    fn handle_printf(&mut self) -> Result<(), VmError> {
         // Pop format string address first (was pushed last by caller, conceptually)
         let format_addr_val = self.pop()?;

         // Read format string from unified memory
         let format_c_str = self.read_c_string(format_addr_val, 1024, "printf format string")?;
         let format_str = format_c_str.to_str()
             .map_err(|e| VmError::CStringError(format!("Invalid UTF-8 in printf format string: {}", e)))?;

         // Process format string, popping actual arguments from stack as needed.
         let mut chars_iter = format_str.chars().peekable();
         let mut output_buffer = String::new(); // Buffer output

         while let Some(c) = chars_iter.next() {
             if c == '%' {
                 match chars_iter.next() {
                     Some('d') => { // Integer
                         let arg_int_value = self.pop()?;
                         output_buffer.push_str(&arg_int_value.to_string());
                     }
                     Some('c') => { // Character
                         let arg_char_value = self.pop()?;
                         if let Some(ch) = std::char::from_u32((arg_char_value & 0xFF) as u32) {
                            output_buffer.push(ch);
                         } else {
                            output_buffer.push('?'); // Invalid char replacement
                         }
                     }
                     Some('s') => { // String
                         let arg_str_addr = self.pop()?;
                         // Read string argument from unified memory
                         let arg_c_str = self.read_c_string(arg_str_addr, 4096, "printf %s argument")?;
                         output_buffer.push_str(&String::from_utf8_lossy(arg_c_str.to_bytes()));
                     }
                     Some('%') => { // Literal '%'
                         output_buffer.push('%');
                     }
                     Some(other) => { // Unrecognized specifier - print literally
                          output_buffer.push('%'); output_buffer.push(other);
                     }
                     None => { // String ends with '%'
                          output_buffer.push('%');
                     }
                 }
             } else {
                 output_buffer.push(c);
             }
         }

         // Print the composed string
         print!("{}", output_buffer);
         use std::io::{self, Write};
         io::stdout().flush().map_err(|e| VmError::IoError(format!("stdout flush failed: {}", e)))?;

         // Set return value in AX (number of bytes printed)
         self.ax = output_buffer.len() as Value; // C printf returns bytes written
         Ok(())
    }


    // --- Debugging Helpers ---
     #[allow(dead_code)]
    pub fn dump_registers(&self) {
        println!("--- Registers ---");
        println!("  PC: 0x{:08X} ({}) [Code Index]", self.pc, self.pc);
        println!("  SP: 0x{:08X} ({}) [Mem Word Index]", self.sp, self.sp);
        println!("  BP: 0x{:08X} ({}) [Mem Word Index]", self.bp, self.bp);
        println!("  AX: 0x{:08X} ({})", self.ax, self.ax);
        println!("  HP: 0x{:08X} ({}) [Mem Word Index]", self.heap_ptr, self.heap_ptr);
        println!("-----------------");
    }

    #[allow(dead_code)]
    pub fn dump_stack(&self, count: usize) {
        println!("--- Stack Top (max {} words, SP word idx -> 0x{:X}) ---", count, self.sp);
        let start_word_idx: usize = self.sp; // SP points to the next free slot (lower addr)
        let end_word_idx = start_word_idx.saturating_add(count).min(self.memory_size_words);

        if start_word_idx >= self.memory_size_words {
            println!("  (SP 0x{:X} points at or past end of memory 0x{:X})", start_word_idx, self.memory_size_words);
        } else if start_word_idx >= end_word_idx && count > 0 {
             println!("  (SP 0x{:X} is at or near end 0x{:X}, nothing to show)", start_word_idx, end_word_idx);
        } else if count == 0 {
             println!("  (Count is zero)");
        } else {
            // Iterate from SP upwards towards higher indices (conceptually lower stack slots)
            for i in start_word_idx..end_word_idx {
                 // Bounds check just in case
                 if i < self.memory.len() {
                     let byte_addr = i * VALUE_SIZE_BYTES;
                     println!("  [Mem Word 0x{:X} / Byte 0x{:X}] 0x{:08X} ({})", i, byte_addr, self.memory[i], self.memory[i]);
                 } else {
                     println!("  [Mem Word 0x{:X}] (Out of bounds!)", i);
                     break;
                 }
            }
        }
        println!("------------------------------------------");
    }

     // Removed: dump_data_segment

      #[allow(dead_code)]
     pub fn dump_heap(&self, count: usize) {
         // Dumps from start of heap (word index 0 or after data) up to heap_ptr or count words
         println!("--- Heap Start (max {} words, HP word idx -> 0x{:X}) ---", count, self.heap_ptr);
         // Figure out where the static data ended and heap potentially began
         // For simplicity, let's just dump from word index 0 up to min(count, heap_ptr)
         // A more accurate dump would need the exact size of the initial data copied.
         let start_word_idx: usize = 0; // Start from memory beginning
         let end_word_idx = start_word_idx.saturating_add(count).min(self.heap_ptr); // Show up to count words or current heap ptr

         if self.heap_ptr == 0 {
            println!("  (Heap pointer is at 0, likely empty or only static data exists)");
         } else if start_word_idx >= end_word_idx && count > 0 {
             println!("  (Heap Empty or HP <= start index 0)");
         } else if count == 0 {
            println!("  (Count is zero)");
         } else {
            for i in start_word_idx..end_word_idx {
                if i < self.memory.len() { // Bounds check
                     let byte_addr = i * VALUE_SIZE_BYTES;
                     println!("  [Mem Word 0x{:X} / Byte 0x{:X}] 0x{:08X} ({})", i, byte_addr, self.memory[i], self.memory[i]);
                } else {
                     println!("  [Mem Word 0x{:X}] (Out of bounds!)", i);
                     break;
                }
            }
         }
         println!("-----------------------------------------");
     }

    /// Dumps a specific section of the unified memory.
    /// `start_word_addr`: The starting word index.
    /// `num_words`: The number of words to dump.
    #[allow(dead_code)]
    pub fn dump_memory_section(&self, start_word_addr: usize, num_words: usize) -> Result<(), VmError> {
        println!("--- Memory Section Dump (Start Word: 0x{:X}, Count: {}) ---", start_word_addr, num_words);

        if num_words == 0 {
            println!("  (Count is zero)");
            println!("---------------------------------------------------------");
            return Ok(());
        }

        let end_word_addr = start_word_addr.checked_add(num_words).ok_or_else(|| VmError::ArithmeticError("Address overflow calculating dump end".to_string()))?;

        // Validate range against total memory size in words
        if start_word_addr >= self.memory_size_words {
             println!("  (Start address 0x{:X} is out of bounds [0x0 - 0x{:X}])", start_word_addr, self.memory_size_words);
             println!("---------------------------------------------------------");
            return Err(VmError::MemoryAccessOutOfBounds { address: (start_word_addr * VALUE_SIZE_BYTES) as Value, size: num_words * VALUE_SIZE_BYTES, op_name: "dump_memory_section".to_string() });
        }

        // Adjust end address if it goes past the end of memory
        let effective_end_word_addr = end_word_addr.min(self.memory_size_words);

         if start_word_addr >= effective_end_word_addr {
             println!("  (Calculated end address 0x{:X} is not after start address 0x{:X})", effective_end_word_addr, start_word_addr);
         } else {
            for i in start_word_addr..effective_end_word_addr {
                 // Direct index access is safe due to range checks above
                 let value = self.memory[i];
                 let byte_addr = i * VALUE_SIZE_BYTES;
                 println!("  [Mem Word 0x{:06X} / Byte 0x{:08X}] 0x{:08X} ({})", i, byte_addr, value, value);
            }
         }

        println!("---------------------------------------------------------");
        Ok(())
    }

} // impl VirtualMachine