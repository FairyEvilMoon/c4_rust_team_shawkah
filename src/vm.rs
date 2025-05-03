// src/vm.rs

use std::convert::TryFrom;
// Required for libc calls and C string handling
use std::ffi::CString; // Removed unused CStr
// Removed: use std::os::unix::ffi::OsStrExt; // This is Unix-specific and wasn't strictly needed

// --- Types and Constants ---

pub type Value = i32; // VM uses 32-bit integers
pub const DEFAULT_MEM_SIZE_WORDS: usize = 256 * 1024; // 256k words = 1MB for i32
pub const DEFAULT_MEM_SIZE_BYTES: usize = DEFAULT_MEM_SIZE_WORDS * std::mem::size_of::<Value>();
// Stack grows downwards from high memory in this model
pub const STACK_TOP_ADDR_WORDS: usize = DEFAULT_MEM_SIZE_WORDS;

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

    // C4 Syscalls/Library Functions mapped to Opcodes
    Open = 34, Read = 35, Clos = 36, Prtf = 37, Malc = 38, Free = 39, Mset = 40, Mcmp = 41,
    // NOTE: C4's EXIT is mapped to opcode 42, but we use Instruction::Exit (opcode 3) for VM control.
    // The parser should map C's `exit()` call to our Instruction::Exit.
    // We will handle the PRTF opcode here explicitly, although the Call instruction
    // handler might also check for a magic address like -1 if desired.
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
            40 => Ok(Instruction::Mset), 41 => Ok(Instruction::Mcmp),
            // Note: C4 EXIT (42) is handled by mapping `exit()` in parser to our `Instruction::Exit` (3)

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
    HeapOverflow, // For simulated malloc
    OperandExpected,
    ArithmeticError(String),
    InvalidJumpTarget(Value),
    MemoryAccessOutOfBounds { address: Value, memory_type: String },
    InvalidShiftAmount(Value),
    DataSegmentAccessError(String),
    InvalidSyscall(Value), // Keep for potential future use
    InvalidFileDescriptor(Value),
    NullPointerAccess(String),
    CStringError(String), // Errors reading C strings from memory
    IoError(String), // Wrapper for underlying IO errors
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         match self {
            VmError::InvalidInstruction(v) => write!(f, "Invalid instruction opcode: {}", v),
            VmError::PcOutOfBounds => write!(f, "Program counter out of code bounds"),
            VmError::StackOverflow => write!(f, "Stack overflow"),
            VmError::StackUnderflow => write!(f, "Stack underflow"),
            VmError::HeapOverflow => write!(f, "Heap overflow (simulated malloc failed)"),
            VmError::OperandExpected => write!(f, "Operand expected, but reached end of code"),
            VmError::ArithmeticError(s) => write!(f, "Arithmetic error: {}", s),
            VmError::InvalidJumpTarget(v) => write!(f, "Invalid jump target address in code: {}", v),
            VmError::MemoryAccessOutOfBounds{ address, memory_type } => write!(f, "{} memory access out of bounds at address: 0x{:X} ({})", memory_type, address, address),
            VmError::InvalidShiftAmount(v) => write!(f, "Invalid shift amount: {}", v),
            VmError::DataSegmentAccessError(s) => write!(f, "Data Segment access error: {}", s),
            VmError::InvalidSyscall(v) => write!(f, "Invalid syscall number: {}", v),
            VmError::InvalidFileDescriptor(v) => write!(f, "Invalid file descriptor: {}", v),
            VmError::NullPointerAccess(s) => write!(f, "Null pointer access during {}", s),
            VmError::CStringError(s) => write!(f, "C String error: {}", s),
            VmError::IoError(s) => write!(f, "I/O Error: {}", s),
        }
    }
}
impl std::error::Error for VmError {}


// --- Virtual Machine ---
#[derive(Debug)]
pub struct VirtualMachine {
    // Registers
    pc: usize,
    pub sp: usize, // Stack pointer (word index, starts high)
    bp: usize, // Base pointer (word index)
    ax: Value, // Accumulator

    // Memory
    code: Vec<Value>,
    memory: Vec<Value>,    // Combined Stack/Heap area (words)
    data_segment: Vec<u8>, // Static data (bytes)
    heap_ptr: usize,       // Simulated heap pointer (word index, starts low)

    // State
    running: bool,
    code_size: usize,
    pub memory_size_words: usize,
}


impl VirtualMachine {
    /// Creates a new VM with code, data segment, and default memory size.
    pub fn new(code: Vec<Value>, data_segment: Vec<u8>) -> Self {
        Self::with_memory_size(code, data_segment, DEFAULT_MEM_SIZE_WORDS)
    }

    /// Creates a new VM with specific memory size (in words).
    pub fn with_memory_size(code: Vec<Value>, data_segment: Vec<u8>, memory_words: usize) -> Self {
        if memory_words == 0 {
            panic!("Memory size must be at least 1 word");
        }
        let memory = vec![0; memory_words]; // Initialize memory with zeros
        let code_size = code.len();
        let initial_sp = memory_words; // SP starts just *past* the end (stack grows down)

        VirtualMachine {
            pc: 0,
            sp: initial_sp,
            bp: initial_sp, // BP initially same as SP
            ax: 0,
            code,
            memory,
            data_segment,
            heap_ptr: 0, // Heap starts at the beginning of memory
            running: false,
            code_size,
            memory_size_words: memory_words,
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

    // --- Stack Operations ---
    #[inline]
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        // Stack grows down, check collision with heap_ptr
        if self.sp == 0 || self.sp <= self.heap_ptr {
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1;
            self.memory[self.sp] = value;
            Ok(())
        }
    }

    #[inline]
    fn pop(&mut self) -> Result<Value, VmError> {
        // Underflow if SP is at or beyond the initial top
        if self.sp >= self.memory_size_words {
            Err(VmError::StackUnderflow)
        } else {
            let value = self.memory[self.sp];
            self.sp += 1;
            Ok(value)
        }
    }

    // --- Memory Validation and Access ---

    // Accesses memory as words (Value)
    #[inline]
    fn validate_mem_word_addr(&self, addr_val: Value, op_name: &str) -> Result<usize, VmError> {
        if addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: format!("{} word", op_name) });
        }
        let addr_usize = addr_val as usize;
        // Check against memory size in WORDS
        if addr_usize >= self.memory_size_words {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: format!("{} word", op_name) });
        }
        Ok(addr_usize)
    }

    // Accesses memory as bytes, using byte address
    #[inline]
    fn get_mem_byte(&self, byte_addr_val: Value, op_name: &str) -> Result<u8, VmError> {
        if byte_addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, memory_type: format!("{} byte", op_name) });
        }
        let byte_addr = byte_addr_val as usize;
        let word_index = byte_addr / std::mem::size_of::<Value>();
        let byte_offset = byte_addr % std::mem::size_of::<Value>();

        // Check against memory size in WORDS
        if word_index >= self.memory_size_words {
             return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, memory_type: format!("{} byte (word boundary check)", op_name) });
        }

        let word = self.memory[word_index];
        // Assuming little-endian byte order within the Value (i32)
        Ok((word >> (byte_offset * 8)) as u8)
    }

    #[inline]
    fn set_mem_byte(&mut self, byte_addr_val: Value, byte_val: u8, op_name: &str) -> Result<(), VmError> {
        if byte_addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, memory_type: format!("{} byte", op_name) });
        }
        let byte_addr = byte_addr_val as usize;
        let word_index = byte_addr / std::mem::size_of::<Value>();
        let byte_offset = byte_addr % std::mem::size_of::<Value>();

        // Check against memory size in WORDS
        if word_index >= self.memory_size_words {
             return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr_val, memory_type: format!("{} byte (word boundary check)", op_name) });
        }

        let word = &mut self.memory[word_index];
        let mask = !(0xFF << (byte_offset * 8));
        *word = (*word & mask) | ((byte_val as Value) << (byte_offset * 8));
        Ok(())
    }

     // Helper to get a mutable slice of the VM's memory (for read/write syscalls, memset, memcmp)
     // Takes byte address and byte count. Validates bounds.
    fn get_memory_slice_mut(&mut self, byte_addr: Value, count: Value, op_name: &str) -> Result<&mut [u8], VmError> {
        if byte_addr < 0 || count < 0 {
            // Check for negative address or count first
            let invalid_addr = if byte_addr < 0 { byte_addr } else { count }; // Report the problematic value
             return Err(VmError::MemoryAccessOutOfBounds { address: invalid_addr, memory_type: format!("{} slice (negative address/count)", op_name) });
        }
        let start_byte = byte_addr as usize;
        let num_bytes = count as usize;
        let end_byte = start_byte.checked_add(num_bytes).ok_or_else(|| VmError::ArithmeticError(format!("Address overflow calculating end of {} slice: {} + {}", op_name, start_byte, num_bytes)))?;

        // Check if the byte range is within the total byte capacity of the memory
        let total_mem_bytes = self.memory_size_words.checked_mul(std::mem::size_of::<Value>())
            .ok_or_else(|| VmError::ArithmeticError("Overflow calculating total memory bytes".to_string()))?;

        if end_byte > total_mem_bytes {
            // Report the address that goes out of bounds (end_byte - 1)
            let oob_addr = (end_byte - 1) as Value;
            return Err(VmError::MemoryAccessOutOfBounds { address: oob_addr, memory_type: format!("{} slice end", op_name) });
        }
         // Handle zero-length slice requested at the very end of memory
        if start_byte == total_mem_bytes && num_bytes == 0 {
             return Ok(&mut []);
        }
        // Check start boundary as well (although end_byte > total_mem_bytes should cover it unless num_bytes is 0)
        if start_byte > total_mem_bytes {
             return Err(VmError::MemoryAccessOutOfBounds { address: byte_addr, memory_type: format!("{} slice start", op_name) });
        }


        // Get a mutable slice of the underlying Vec<Value> as bytes
        let memory_bytes: &mut [u8] = unsafe {
            std::slice::from_raw_parts_mut(
                self.memory.as_mut_ptr() as *mut u8,
                total_mem_bytes // Use the actual total byte capacity
            )
        };

        // Slice the byte slice - this is safe because we validated bounds above
        Ok(&mut memory_bytes[start_byte..end_byte])
    }


    #[inline]
    fn validate_data_segment_address(&self, addr_val: Value) -> Result<usize, VmError> {
        if addr_val < 0 {
             return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Data Segment".to_string()});
        }
        let addr_usize = addr_val as usize;
        if addr_usize >= self.data_segment.len() {
             return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: "Data Segment".to_string()});
        }
        Ok(addr_usize)
    }

    #[inline]
    fn validate_jump_target(&self, target_val: Value) -> Result<usize, VmError> {
         if target_val < 0 { return Err(VmError::InvalidJumpTarget(target_val)); }
         let target_pc = target_val as usize;
         // Allow jump *to* the end (pc == code_size), which signals program end
         if target_pc > self.code_size { return Err(VmError::InvalidJumpTarget(target_val)); }
         Ok(target_pc)
    }

    /// Reads a null-terminated C string from memory (stack/heap or data segment).
    /// `addr_val` is the starting byte address.
    fn read_c_string(&self, addr_val: Value, max_len: usize, location: &str) -> Result<CString, VmError> {
        if addr_val < 0 {
            return Err(VmError::MemoryAccessOutOfBounds { address: addr_val, memory_type: format!("{} C string", location) });
        }
        if addr_val == 0 { // Consider NULL pointer
            return Err(VmError::NullPointerAccess(format!("reading {} C string", location)));
        }

        let mut bytes = Vec::new();
        let mut current_addr = addr_val;

        loop {
            let byte = match location {
                 "Data Segment" => {
                    let byte_idx = self.validate_data_segment_address(current_addr)?;
                    self.data_segment[byte_idx]
                 }
                 "Memory" => {
                    // Use get_mem_byte for stack/heap access (byte address)
                    self.get_mem_byte(current_addr, "C string in Memory")?
                 }
                 _ => panic!("Invalid location specified for read_c_string: {}", location),
            };

            if byte == 0 { break; } // Null terminator found
            bytes.push(byte);
            current_addr = current_addr.checked_add(1).ok_or_else(|| VmError::ArithmeticError("Address overflow reading C String".to_string()))?;

            if bytes.len() >= max_len {
                 return Err(VmError::CStringError(format!("String from {} at 0x{:X} exceeded max length {}", location, addr_val, max_len)));
            }
        }

        // Check if CString::new can actually fail (e.g., internal null bytes)
        CString::new(bytes).map_err(|e| VmError::CStringError(format!("Invalid byte sequence in C string from {} at 0x{:X}: {}", location, addr_val, e)))
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
            // Prevent re-entry if already running (e.g., nested calls from error handling)
            // This might need more sophisticated handling depending on use case.
             return Err(VmError::IoError("VM already running".to_string())); // Use IoError or a new variant
        }
        self.running = true;
        let mut final_result: Result<Value, VmError> = Ok(self.ax); // Default result if loop doesn't run

        while self.running {
             // Check PC bounds *before* fetching instruction
            if self.pc >= self.code_size {
                 if self.pc == self.code_size {
                     // Normal termination: reached end of code
                     self.running = false;
                     final_result = Ok(self.ax); // Use AX value at termination
                 } else {
                     // Error: PC somehow went beyond code size
                     final_result = Err(VmError::PcOutOfBounds);
                     self.running = false;
                 }
                 break; // Exit loop
             }

            // Store PC before execution in case of error reporting
            let current_pc = self.pc;
            match self.fetch_instruction().and_then(|instr| self.execute(instr)) {
                Ok(()) => {
                    // Instruction executed successfully, continue loop
                    // Check running flag again in case execute set it to false (e.g. Exit)
                    if !self.running {
                       final_result = Ok(self.ax); // Exit instruction sets AX
                    }
                }
                Err(e) => {
                    // An error occurred
                    eprintln!("VM Error at PC=0x{:X}: {}", current_pc, e); // Print error immediately for debugging
                    self.dump_registers(); // Optionally dump state on error
                    self.dump_stack(10);
                    final_result = Err(e);
                    self.running = false; // Stop execution on any error
                }
            }
        }

        self.running = false; // Ensure running is false on exit
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
                // C function exit() places return code in AX before calling this.
                // This instruction stops the VM. The run loop will return current AX.
                self.running = false;
            }

            // --- Arithmetic ---
            Instruction::Add => { let op = self.pop()?; self.ax = self.checked_add(op, self.ax)?; }
            Instruction::Sub => { let op = self.pop()?; self.ax = self.checked_sub(op, self.ax)?; } // Order op - ax
            Instruction::Mul => { let op = self.pop()?; self.ax = self.checked_mul(op, self.ax)?; }
            Instruction::Div => { let op = self.pop()?; self.ax = self.checked_div(op, self.ax)?; } // Order op / ax
            Instruction::Mod => { let op = self.pop()?; self.ax = self.checked_rem(op, self.ax)?; } // Order op % ax

            // --- Bitwise/Logical ---
            Instruction::Or => { let op = self.pop()?; self.ax = op | self.ax; }
            Instruction::Xor => { let op = self.pop()?; self.ax = op ^ self.ax; }
            Instruction::And => { let op = self.pop()?; self.ax = op & self.ax; }
            Instruction::Shl => { let shift_amount = self.pop()?; self.ax = self.checked_shl(self.ax, shift_amount)?; } // Order ax << amount
            Instruction::Shr => { let shift_amount = self.pop()?; self.ax = self.checked_shr(self.ax, shift_amount)?; } // Order ax >> amount

            // --- Relational/Equality (Order: op CMP ax) ---
            Instruction::Eq => { let op = self.pop()?; self.ax = if op == self.ax { 1 } else { 0 }; }
            Instruction::Ne => { let op = self.pop()?; self.ax = if op != self.ax { 1 } else { 0 }; }
            Instruction::Lt => { let op = self.pop()?; self.ax = if op < self.ax { 1 } else { 0 }; }
            Instruction::Gt => { let op = self.pop()?; self.ax = if op > self.ax { 1 } else { 0 }; }
            Instruction::Le => { let op = self.pop()?; self.ax = if op <= self.ax { 1 } else { 0 }; }
            Instruction::Ge => { let op = self.pop()?; self.ax = if op >= self.ax { 1 } else { 0 }; }

            // --- Unary ---
            Instruction::Neg => { self.ax = self.checked_neg(self.ax)?; }
            Instruction::Not => { self.ax = if self.ax == 0 { 1 } else { 0 }; } // Logical NOT

            // --- Control Flow ---
            Instruction::Jmp => { let target = self.fetch_operand()?; self.pc = self.validate_jump_target(target)?; }
            Instruction::Jz => { let target = self.fetch_operand()?; if self.ax == 0 { self.pc = self.validate_jump_target(target)?; } }
            Instruction::Jnz => { let target = self.fetch_operand()?; if self.ax != 0 { self.pc = self.validate_jump_target(target)?; } }

            // --- Function Call ---
            Instruction::Call => {
                let target_addr_val = self.fetch_operand()?;
                let target_pc = self.validate_jump_target(target_addr_val)?;
                 // Push *next* instruction address as return address
                 let return_pc = self.pc as Value;
                 self.push(return_pc)?;
                 self.pc = target_pc; // Jump
            }
            Instruction::Ent => {
                let locals_size_words_val = self.fetch_operand()?;
                if locals_size_words_val < 0 { return Err(VmError::ArithmeticError(format!("ENT operand must be non-negative: {}", locals_size_words_val))); }
                let locals_size_words = locals_size_words_val as usize;

                self.push(self.bp as Value)?; // Push old BP
                self.bp = self.sp;            // New BP is current SP (before allocating locals)

                // Check if allocating locals would overflow stack (hit heap or go below 0)
                let next_sp = self.sp.checked_sub(locals_size_words);
                match next_sp {
                     Some(sp) if sp >= self.heap_ptr => { // Check against heap ptr
                         self.sp = sp;
                         // Optional: Zero out local variable space? C doesn't guarantee this.
                         // for i in 0..locals_size_words { self.memory[self.sp + i] = 0; }
                     }
                     _ => { // Stack overflow
                         // Restore BP before erroring? Maybe not necessary as VM stops.
                         // self.bp = self.pop()? as usize; // This might fail if stack is truly messed up
                         return Err(VmError::StackOverflow);
                     }
                 }
            }
            Instruction::Adj => {
                let arg_count_val = self.fetch_operand()?;
                 if arg_count_val < 0 { return Err(VmError::ArithmeticError(format!("ADJ operand must be non-negative: {}", arg_count_val))); }
                 let arg_count = arg_count_val as usize;

                 // Adjust SP upwards to remove arguments pushed by caller
                 let new_sp = self.sp.checked_add(arg_count);
                 match new_sp {
                     // Check if new SP is valid (doesn't go beyond original memory top)
                     Some(sp) if sp <= self.memory_size_words => self.sp = sp,
                     _ => return Err(VmError::StackUnderflow), // Removing too many args?
                 }
            }
            Instruction::Lev => { // Leave function
                 // Check if BP is valid before using it
                 if self.bp >= self.memory_size_words {
                     // This suggests BP corruption or stack underflow before LEV
                     return Err(VmError::StackUnderflow); // Or a specific BP error
                 }
                 // Deallocate locals and args space managed by ENT/ADJ within the callee
                 self.sp = self.bp; // SP = BP (BP points *before* locals/old BP)

                 // Restore caller's BP
                 self.bp = self.pop()? as usize; // Pop old BP (need validation?)

                 // Pop return address
                 let return_pc_val = self.pop()?;
                 self.pc = self.validate_jump_target(return_pc_val)?; // Jump back
            }

            // --- Memory Access ---
            Instruction::Lea => { // Load Effective Address (bp + offset)
                let offset = self.fetch_operand()?;
                 // BP is a word index, offset is typically words too in C4 stack frames
                 // Let's treat offset as a raw value to add to BP (word index)
                 // The *result* is an address (potentially byte address depending on usage?)
                 // C4 LEA likely calculates stack address (word index). Let's assume result is word index based.
                 // Need to be careful if LEA is used for global/data addresses.
                 // Check for overflow during address calculation.
                 // Assume BP is a valid word index because ENT/LEV manage it
                 let base = self.bp as i64; // Use i64 to prevent intermediate overflow
                 let offset64 = offset as i64;
                 let calculated_addr64 = base.checked_add(offset64).ok_or_else(|| VmError::ArithmeticError("LEA address calculation overflow".to_string()))?;

                 // Should the result be validated against memory bounds? LEA itself usually doesn't fail.
                 // The *use* of the address (LI, SI) will fail if invalid.
                 // Convert back to Value (i32) - potential truncation if i64 was needed
                 if calculated_addr64 < (Value::MIN as i64) || calculated_addr64 > (Value::MAX as i64) {
                     return Err(VmError::ArithmeticError("LEA result out of Value range".to_string()));
                 }
                 self.ax = calculated_addr64 as Value;
            }
            Instruction::Li => { // Load Integer (from word address in AX)
                let addr = self.validate_mem_word_addr(self.ax, "LI")?;
                self.ax = self.memory[addr];
            }
            Instruction::Lc => { // Load Character (from byte address in AX)
                let byte_val = self.get_mem_byte(self.ax, "LC")?;
                self.ax = byte_val as Value; // Zero-extend char to Value
            }
            Instruction::Si => { // Store Integer (AX into word address from stack)
                 let addr_val = self.pop()?;
                 let addr = self.validate_mem_word_addr(addr_val, "SI")?;
                 self.memory[addr] = self.ax;
            }
            Instruction::Sc => { // Store Character (AX low byte into byte address from stack)
                 let addr_val = self.pop()?;
                 // Address needs validation *before* potential access
                 let byte_to_store = (self.ax & 0xFF) as u8;
                 self.set_mem_byte(addr_val, byte_to_store, "SC")?;
            }

            // --- Syscalls ---
            // Note: These syscalls assume arguments are pushed in C order (last arg pushed first)
            // Stack before syscall: [..., argN, ..., arg2, arg1] -> pops arg1, arg2, ...
            Instruction::Open => { // open(pathname, flags) -> fd ; Stack: [..., pathname_addr, flags]
                let flags = self.pop()?;
                let pathname_addr = self.pop()?;
                // Assume pathname is in general 'Memory' (stack/heap/global mapped area)
                let pathname_c = self.read_c_string(pathname_addr, 1024, "Memory")?;

                // Use libc::open (careful with flags interpretation if different from host OS)
                // For basic C4, direct mapping might be okay.
                self.ax = unsafe { libc::open(pathname_c.as_ptr(), flags as libc::c_int) };
                // Check for errors? libc::open returns -1 on error.
                // if self.ax == -1 { Err(VmError::IoError(format!("open failed for '{}'", pathname_c.to_string_lossy())))? }
            }
            Instruction::Read => { // read(fd, buf_addr, count) -> bytes_read ; Stack: [..., fd, buf_addr, count]
                let count_val = self.pop()?;
                let buf_addr = self.pop()?;
                let fd = self.pop()?;

                if fd < 0 { return Err(VmError::InvalidFileDescriptor(fd)); }
                if count_val < 0 { return Err(VmError::ArithmeticError(format!("READ count cannot be negative: {}", count_val))); }
                if buf_addr == 0 { return Err(VmError::NullPointerAccess("READ buffer".to_string())); }

                let count = count_val as usize; // Convert to usize for slice index and libc call
                let buf_slice = self.get_memory_slice_mut(buf_addr, count_val, "READ")?;

                let bytes_read = unsafe {
                    libc::read(fd, buf_slice.as_mut_ptr() as *mut libc::c_void, count.try_into().unwrap())
                };
                 // Returns bytes read (>=0) on success, -1 on error
                 self.ax = bytes_read as Value;
                 // if self.ax == -1 { Err(VmError::IoError("read failed".to_string()))? }
            }
            Instruction::Clos => { // close(fd) -> 0 or -1 ; Stack: [..., fd]
                let fd = self.pop()?;
                if fd < 0 { return Err(VmError::InvalidFileDescriptor(fd)); }
                self.ax = unsafe { libc::close(fd) };
                 // Returns 0 on success, -1 on error
                 // if self.ax == -1 { Err(VmError::IoError("close failed".to_string()))? }
            }
            Instruction::Prtf => { // printf(format_addr, arg1, arg2, ...) -> count
                // Stack: [..., format_addr, arg1, arg2, ...]
                // Arguments are popped inside the handler in reverse order they appear on stack
                self.handle_printf()?;
            }
            Instruction::Malc => { // malloc(size) -> ptr ; Stack: [..., size]
                let size_bytes = self.pop()?;
                if size_bytes <= 0 {
                    self.ax = 0; // malloc(0) or malloc(<0) returns NULL
                } else {
                    // Simulate allocation from heap_ptr
                    // Word alignment: C malloc guarantees alignment suitable for any fundamental type.
                    // For simplicity, let's align to word boundary (size_of::<Value>).
                    let size_bytes_usize = size_bytes as usize;
                    let required_bytes_aligned = (size_bytes_usize + std::mem::size_of::<Value>() - 1) & !(std::mem::size_of::<Value>() - 1);
                    let required_words = required_bytes_aligned / std::mem::size_of::<Value>();

                    let new_heap_ptr = self.heap_ptr.checked_add(required_words);

                    match new_heap_ptr {
                         // Check collision with stack pointer (heap grows up, stack grows down)
                         Some(next_hp) if next_hp <= self.sp => { // Use <= because SP points to the *next* free slot when growing down
                             let alloc_addr_bytes = self.heap_ptr * std::mem::size_of::<Value>();
                             self.ax = alloc_addr_bytes as Value; // Return byte address of allocation start
                             self.heap_ptr = next_hp; // Update heap pointer

                             // Optional: Zero out allocated memory? C malloc doesn't guarantee this.
                             // let alloc_slice = self.get_memory_slice_mut(self.ax, required_bytes_aligned as Value, "MALLOC zeroing")?;
                             // alloc_slice.fill(0);

                         }
                         _ => { // Heap overflow (collision or usize overflow)
                            self.ax = 0; // C returns NULL (0) on failure
                            // Optionally return VmError::HeapOverflow instead, but C programs expect NULL
                            // return Err(VmError::HeapOverflow);
                         }
                    }
                }
            }
            Instruction::Free => { // free(ptr) -> void ; Stack: [..., ptr]
                let _ptr = self.pop()?; // Pop pointer from stack
                // Our bump allocator doesn't support freeing individual blocks. No-op.
                self.ax = 0; // free has no return value, but set AX to 0 for predictability
            }
            Instruction::Mset => { // memset(dest_addr, char_val, count) -> dest_addr ; Stack: [..., dest_addr, char_val, count]
                 let count_val = self.pop()?;
                 let char_val = self.pop()?; // Value containing the byte in its lower 8 bits
                 let dest_addr = self.pop()?;

                 if count_val < 0 { return Err(VmError::ArithmeticError(format!("MSET count cannot be negative: {}", count_val))); }
                 if dest_addr == 0 { return Err(VmError::NullPointerAccess("MSET destination".to_string())); }
                 // A count of 0 is valid, should do nothing and return dest_addr

                 if count_val > 0 {
                    let dest_slice = self.get_memory_slice_mut(dest_addr, count_val, "MSET")?;
                    let byte_to_set = (char_val & 0xFF) as u8;
                    dest_slice.fill(byte_to_set);
                 }

                 self.ax = dest_addr; // memset returns the destination pointer
            }
            Instruction::Mcmp => { // memcmp(addr1, addr2, count) -> comparison_result ; Stack: [..., addr1, addr2, count]
                 let count_val = self.pop()?;
                 let addr2 = self.pop()?;
                 let addr1 = self.pop()?;

                 if count_val < 0 { return Err(VmError::ArithmeticError(format!("MCMP count cannot be negative: {}", count_val))); }
                 if addr1 == 0 { return Err(VmError::NullPointerAccess("MCMP address 1".to_string())); }
                 if addr2 == 0 { return Err(VmError::NullPointerAccess("MCMP address 2".to_string())); }
                 // A count of 0 is valid, should return 0

                 let count = count_val as usize;
                 let mut result: Value = 0;

                 // Avoid slice borrowing issues by reading bytes directly
                 for i in 0..count {
                    // Calculate addresses for each byte carefully, checking for overflow
                    let current_addr1 = addr1.checked_add(i as Value).ok_or_else(|| VmError::ArithmeticError("MCMP address 1 overflow".to_string()))?;
                    let current_addr2 = addr2.checked_add(i as Value).ok_or_else(|| VmError::ArithmeticError("MCMP address 2 overflow".to_string()))?;

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

        } // end match instruction
        Ok(())
    }

    /// Handles the printf logic (separated for clarity)
    /// Assumes arguments are on the stack [..., format_addr, argN, ..., arg1]
    /// Pops arguments as needed.
    fn handle_printf(&mut self) -> Result<(), VmError> {
         // 1. Get format string address (last argument pushed, so first one accessible conceptually)
         // BUT, printf needs it first. It's on top relative to args, but deep relative to current SP.
         // The C calling convention puts args on stack, then calls. The format addr is usually passed like other args.
         // Let's assume BP points to the frame: [ret addr][old bp][local0]..[arg0][arg1]...[argN]
         // C4 spec might clarify exact layout. Assuming args are accessible relative to BP or SP *before* printf call setup.
         // A simpler VM might assume args are just pushed before CALL/syscall: [argN]...[arg1][format_addr] <- SP before Prtf instruction
         // Let's stick to the simple stack model: pop format_addr first.
         let str_addr_val = self.pop()?; // Pop format string address

         // Where is the format string? Could be data segment (constants) or memory (dynamic).
         // Let's try data segment first, then memory if that fails or address seems too high.
         // This is heuristic and might be wrong for edge cases.
         let format_c_str = self.read_c_string(str_addr_val, 1024, "Data Segment")
              .or_else(|_| self.read_c_string(str_addr_val, 1024, "Memory")) // Try memory if data segment fails
              .map_err(|e| VmError::CStringError(format!("Failed to read printf format string at 0x{:X}: {}", str_addr_val, e)))?; // Combine errors

         let format_str = format_c_str.to_str()
             .map_err(|e| VmError::CStringError(format!("Invalid UTF-8 in printf format string: {}", e)))?;

         // 2. Process format string, popping actual arguments from stack as needed.
         let mut chars_iter = format_str.chars().peekable();
         let mut output_buffer = String::new(); // Buffer output to print once
         let mut chars_printed_count = 0;

         while let Some(c) = chars_iter.next() {
             if c == '%' {
                 match chars_iter.next() {
                     Some('d') => {
                         let arg_int_value = self.pop()?; // Pop integer argument
                         let formatted_arg = arg_int_value.to_string();
                         output_buffer.push_str(&formatted_arg);
                         chars_printed_count += formatted_arg.len();
                     }
                     Some('c') => { // Handle %c for characters
                         let arg_char_value = self.pop()?; // Pop character argument (as int)
                         // Ensure it's a valid char before push
                         if let Some(ch) = std::char::from_u32((arg_char_value & 0xFF) as u32) {
                            output_buffer.push(ch);
                            chars_printed_count += 1;
                         } else {
                            // Handle invalid char? Print replacement char? Error?
                            output_buffer.push('?'); // Replace invalid byte with '?'
                            chars_printed_count += 1;
                         }
                     }
                     Some('s') => { // Handle %s for strings
                         let arg_str_addr = self.pop()?; // Pop string address (byte address)
                         // Assume string is in Memory (stack/heap) for %s arguments
                         // Could potentially be in Data Segment too. Add similar fallback?
                         let arg_c_str = self.read_c_string(arg_str_addr, 4096, "Memory")
                             .map_err(|e| VmError::CStringError(format!("Failed reading %%s arg at 0x{:X}: {}", arg_str_addr, e)))?;
                         // Convert CString to Rust string slice (&str) lossily if needed
                         let arg_str = String::from_utf8_lossy(arg_c_str.to_bytes());
                         output_buffer.push_str(&arg_str);
                         chars_printed_count += arg_str.len(); // Count bytes or chars? C printf counts bytes. Use len().
                     }
                     Some('%') => {
                         output_buffer.push('%');
                         chars_printed_count += 1;
                     }
                     Some(other) => { // Unrecognized or unsupported specifier - print literally
                          output_buffer.push('%'); output_buffer.push(other); chars_printed_count += 2;
                     }
                     None => { // String ends with '%'
                          output_buffer.push('%'); chars_printed_count += 1;
                     }
                 }
             } else {
                 output_buffer.push(c);
                 chars_printed_count += 1; // Count chars or bytes? Assume bytes for C compatibility. This counts chars. Let's adjust.
                 // Correction: C printf counts bytes written. Need len_utf8.
                 // Let's recalculate count at the end based on the final string bytes.
             }
         }

         // 3. Print the composed string (using Rust's print!)
         // Use print! which goes to stdout, respecting locking
         print!("{}", output_buffer);
         use std::io::{self, Write};
         io::stdout().flush().map_err(|e| VmError::IoError(format!("stdout flush failed: {}", e)))?; // Ensure output is visible

         // 4. Set return value in AX (number of bytes printed) - Recalculate based on bytes
         self.ax = output_buffer.len() as Value; // Use byte length of the final string
         Ok(())
    }


    // --- Debugging Helpers ---
     #[allow(dead_code)]
    pub fn dump_registers(&self) {
        println!("--- Registers ---");
        println!("  PC: 0x{:08X} ({})", self.pc, self.pc);
        println!("  SP: 0x{:08X} ({})", self.sp, self.sp);
        println!("  BP: 0x{:08X} ({})", self.bp, self.bp);
        println!("  AX: 0x{:08X} ({})", self.ax, self.ax);
        println!("  HP: 0x{:08X} ({})", self.heap_ptr, self.heap_ptr); // Heap pointer (word index)
        println!("-----------------");
    }

    #[allow(dead_code)]
    pub fn dump_stack(&self, count: usize) {
        println!("--- Stack Top (max {} entries, SP -> 0x{:X}) ---", count, self.sp);
        // FIX: Add type annotation for `start`
        let start: usize = self.sp;
        // Calculate end bound safely, ensuring it doesn't wrap or exceed memory size
        let end = start.saturating_add(count).min(self.memory_size_words);

        if start >= self.memory_size_words {
            println!("  (SP 0x{:X} points outside memory 0x{:X})", start, self.memory_size_words);
        } else if start >= end && count > 0 {
             println!("  (SP 0x{:X} is at or near end 0x{:X}, nothing to show for count {})", start, end, count);
        } else if count == 0 {
             println!("  (Count is zero)");
        } else {
            // Iterate from SP upwards towards higher addresses (lower in memory)
            for i in start..end {
                 // Check if index `i` is valid *within the loop* just in case `end` calculation was off
                 if i < self.memory.len() {
                     println!("  [Mem Idx 0x{:X}] 0x{:08X} ({})", i, self.memory[i], self.memory[i]);
                 } else {
                     println!("  [Mem Idx 0x{:X}] (Out of bounds!)", i);
                     break; // Stop if we somehow go out of bounds
                 }
            }
        }
        println!("------------------------------------------");
    }

     #[allow(dead_code)]
     pub fn dump_data_segment(&self) {
         println!("--- Data Segment ({} bytes) ---", self.data_segment.len());
         const BYTES_PER_LINE: usize = 16;
         if self.data_segment.is_empty() {
             println!("  (Empty)");
         } else {
            for (i, chunk) in self.data_segment.chunks(BYTES_PER_LINE).enumerate() {
                print!("  {:04X}: ", i * BYTES_PER_LINE);
                for byte in chunk { print!("{:02X} ", byte); }
                // Pad line if it's shorter than BYTES_PER_LINE
                if chunk.len() < BYTES_PER_LINE { for _ in 0..(BYTES_PER_LINE - chunk.len()) { print!("   "); } }
                print!(" |");
                for byte in chunk {
                    // Print printable ASCII chars, otherwise '.'
                    print!("{}", if *byte >= 32 && *byte <= 126 { *byte as char } else { '.' });
                }
                println!("|");
            }
         }
         println!("----------------------------");
     }

      #[allow(dead_code)]
     pub fn dump_heap(&self, count: usize) {
         println!("--- Heap Start (max {} words, HP -> 0x{:X}) ---", count, self.heap_ptr);
         let start_word: usize = 0;
         // Show words from index 0 up to `count` or `heap_ptr`, whichever is smaller
         let end_word = start_word.saturating_add(count).min(self.heap_ptr);

         if start_word >= end_word && count > 0 {
             println!("  (Heap Empty or HP=0)");
         } else if count == 0 {
            println!("  (Count is zero)");
         } else {
            for i in start_word..end_word {
                // Check index validity just in case
                if i < self.memory.len() {
                    println!("  [Mem Idx 0x{:X}] 0x{:08X} ({})", i, self.memory[i], self.memory[i]);
                } else {
                     println!("  [Mem Idx 0x{:X}] (Out of bounds!)", i);
                     break;
                }
            }
         }
         println!("-----------------------------------------");
     }

} // impl VirtualMachine