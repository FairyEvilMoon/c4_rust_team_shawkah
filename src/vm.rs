// src/vm.rs

use std::convert::TryFrom;

// --- Types and Constants ---

pub type Value = i32; // The basic data type for registers, stack, and code operands.
pub const DEFAULT_MEM_SIZE: usize = 1024 * 1024; // Default size for the stack/memory area (1MB).
pub const STACK_TOP: usize = DEFAULT_MEM_SIZE;      // Represents the initial high address for SP/BP. Stack grows downwards.

// --- Instructions ---

/// Represents the opcodes for the C4 virtual machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)] // Use explicit representation matching C4's likely integer opcodes
pub enum Instruction {
    // Core & Flow Control
    Nop = 0,  // No operation
    Imm = 1,  // Load immediate value into AX
    Push = 2, // Push AX onto the stack
    Exit = 3, // Terminate execution

    // Arithmetic (Operate on AX and value popped from stack)
    Add = 4,  // AX = Pop() + AX
    Sub = 5,  // AX = Pop() - AX
    Mul = 6,  // AX = Pop() * AX
    Div = 7,  // AX = Pop() / AX (integer division)
    Mod = 8,  // AX = Pop() % AX (remainder)

    // Unconditional and Conditional Jumps
    Jmp = 9,  // Jump to operand address
    Jz = 10,  // Jump if AX == 0
    Jnz = 11, // Jump if AX != 0

    // Function Call Mechanism
    Call = 12, // Push return address, Jump to operand address
    Ent = 13,  // Create stack frame: Push BP, BP = SP, SP -= operand (locals size)
    Adj = 14,  // Adjust stack pointer after call: SP += operand (arg count)
    Lev = 15,  // Leave stack frame: SP = BP, Pop BP, Pop PC (return)

    // Memory Access (using AX as the address)
    Li = 16,  // Load Integer: AX = Mem[AX]
    Lc = 17,  // Load Character: AX = Mem[AX] & 0xFF (zero-extended)
    Si = 18,  // Store Integer: Mem[AX] = Pop()
    Sc = 19,  // Store Character: Mem[AX] = Pop() & 0xFF
}

impl TryFrom<Value> for Instruction {
    type Error = VmError;

    /// Attempts to convert an integer value into an Instruction.
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Instruction::Nop), 1 => Ok(Instruction::Imm), 2 => Ok(Instruction::Push),
            3 => Ok(Instruction::Exit), 4 => Ok(Instruction::Add), 5 => Ok(Instruction::Sub),
            6 => Ok(Instruction::Mul), 7 => Ok(Instruction::Div), 8 => Ok(Instruction::Mod),
            9 => Ok(Instruction::Jmp), 10 => Ok(Instruction::Jz), 11 => Ok(Instruction::Jnz),
            12 => Ok(Instruction::Call), 13 => Ok(Instruction::Ent), 14 => Ok(Instruction::Adj),
            15 => Ok(Instruction::Lev), 16 => Ok(Instruction::Li), 17 => Ok(Instruction::Lc),
            18 => Ok(Instruction::Si), 19 => Ok(Instruction::Sc),
            _ => Err(VmError::InvalidInstruction(value)), // Unknown opcode
        }
    }
}

// --- Errors ---

/// Represents possible errors during VM execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmError {
    InvalidInstruction(Value),       // Encountered an unknown opcode value.
    PcOutOfBounds,                   // Program Counter went outside the bounds of the code segment.
    StackOverflow,                   // Attempt to push onto a full stack (sp reached 0).
    StackUnderflow,                  // Attempt to pop from an empty stack (sp reached STACK_TOP) or adjust SP beyond STACK_TOP.
    OperandExpected,                 // Reached end of code while fetching an operand for an instruction.
    ArithmeticOverflow,              // Result of an arithmetic operation exceeded i32 bounds.
    InvalidJumpTarget(Value),        // Jump address was negative or outside the code segment bounds.
    DivisionByZero,                  // Attempted division or modulo by zero.
    MemoryAccessOutOfBounds(Value),  // Address used by LI/LC/SI/SC was negative or outside the stack/memory bounds.
    // Add more specific errors if needed, e.g., for negative ENT/ADJ operands
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
         match self {
            VmError::InvalidInstruction(v) => write!(f, "Invalid instruction opcode: {}", v),
            VmError::PcOutOfBounds => write!(f, "Program counter out of bounds"),
            VmError::StackOverflow => write!(f, "Stack overflow"),
            VmError::StackUnderflow => write!(f, "Stack underflow"),
            VmError::OperandExpected => write!(f, "Operand expected, but reached end of code"),
            VmError::ArithmeticOverflow => write!(f, "Arithmetic overflow"),
            VmError::InvalidJumpTarget(v) => write!(f, "Invalid jump target address: {}", v),
            VmError::DivisionByZero => write!(f, "Division by zero"),
            VmError::MemoryAccessOutOfBounds(v) => write!(f, "Memory access out of bounds at address: {}", v),
        }
    }
}

impl std::error::Error for VmError {}


// --- Virtual Machine ---

/// The C4 Virtual Machine implementation.
#[derive(Debug, PartialEq)]
pub struct VirtualMachine {
    // Registers
    pub pc: usize,          // Program Counter: Index of the next instruction in `code`.
    pub sp: usize,          // Stack Pointer: Index of the *next available slot* in `stack`. Grows downwards from `STACK_TOP`.
    pub bp: usize,          // Base Pointer: Index indicating the base of the current stack frame for local variable access.
    pub ax: Value,          // Accumulator Register: Holds intermediate results and operands.

    // Memory Segments
    code: Vec<Value>,       // Code segment: Contains the sequence of instructions and their operands.

    // NOTE: In this simplified VM, the `stack` vector serves as both the runtime stack
    // (for function calls, temporary values) and the general data memory area
    // accessible via LI/LC/SI/SC instructions. Addresses used by these memory
    // instructions are direct indices into this `stack` vector.
    pub stack: Vec<Value>,  // Stack / General Data Memory segment.

    // VM State
    running: bool,          // Flag indicating if the VM should continue execution.
    code_size: usize,       // Cache the size of the code segment for efficient bounds checks.
}

impl VirtualMachine {
    /// Creates a new Virtual Machine with the given code.
    /// Initializes registers and allocates the stack/memory area.
    pub fn new(code: Vec<Value>) -> Self {
        let stack = vec![0; DEFAULT_MEM_SIZE]; // Initialize memory to zeros.
        let code_size = code.len();
        VirtualMachine {
            pc: 0,             // Start execution at the beginning of the code.
            sp: STACK_TOP,     // SP starts pointing just beyond the allocated stack memory.
            bp: STACK_TOP,     // BP starts the same as SP initially.
            ax: 0,             // Accumulator starts at 0.
            code,              // Store the provided code.
            stack,             // Store the allocated stack/memory.
            running: false,    // VM is not running until `run()` is called.
            code_size,         // Cache the code size.
        }
    }

    // --- Fetching ---

    /// Fetches the next `Value` (instruction or operand) from the `code` segment at the current `pc`.
    /// Increments the `pc` and checks for bounds.
    #[inline] // Potentially inline for performance in the run loop.
    fn fetch_value(&mut self) -> Result<Value, VmError> {
        if self.pc >= self.code_size {
            self.running = false; // Stop running if we try to fetch past the end.
            Err(VmError::PcOutOfBounds)
        } else {
            let value = self.code[self.pc];
            self.pc += 1;
            Ok(value)
        }
    }

    /// Fetches the next value specifically as an `Instruction`.
    #[inline]
    fn fetch_instruction(&mut self) -> Result<Instruction, VmError> {
        let value = self.fetch_value()?;
        Instruction::try_from(value) // Convert the fetched value to an Instruction enum.
    }

    /// Fetches the next value specifically as an operand (`Value`).
    /// Provides a more specific error if fetching goes out of bounds.
    #[inline]
    fn fetch_operand(&mut self) -> Result<Value, VmError> {
        self.fetch_value().map_err(|e| match e {
            VmError::PcOutOfBounds => VmError::OperandExpected, // Reaching end of code when expecting an operand.
            other => other,
        })
    }

    // --- Stack Operations ---

    /// Pushes a `value` onto the stack.
    /// Decrements `sp` and checks for stack overflow.
    #[inline]
    fn push(&mut self, value: Value) -> Result<(), VmError> {
        if self.sp == 0 { // If SP is already at the absolute bottom.
            Err(VmError::StackOverflow)
        } else {
            self.sp -= 1; // Stack grows downwards, so decrement SP.
            self.stack[self.sp] = value; // Store the value at the new SP location.
            Ok(())
        }
    }

    /// Pops a `value` from the stack.
    /// Increments `sp` and checks for stack underflow.
    #[inline]
    fn pop(&mut self) -> Result<Value, VmError> {
        if self.sp >= STACK_TOP { // If SP is at or beyond the initial top position.
            Err(VmError::StackUnderflow)
        } else {
            let value = self.stack[self.sp]; // Get the value at the current SP.
            self.sp += 1; // Stack shrinks upwards, so increment SP.
            Ok(value)
        }
    }

    // --- Validation ---

    /// Validates an address for memory access (LI/LC/SI/SC).
    /// Ensures the address is within the bounds of the `stack` vector.
    #[inline]
    fn validate_memory_address(&self, addr: Value) -> Result<usize, VmError> {
        if addr < 0 || (addr as usize) >= self.stack.len() { // Check negative and upper bound.
            Err(VmError::MemoryAccessOutOfBounds(addr))
        } else {
            Ok(addr as usize) // Convert valid address to usize index.
        }
    }

    /// Validates a jump target address (JMP/JZ/JNZ/CALL).
    /// Ensures the address is within the bounds of the `code` segment.
    #[inline]
    fn validate_jump_target(&self, target: Value) -> Result<usize, VmError> {
        if target < 0 || (target as usize) >= self.code_size { // Check negative and upper bound.
            Err(VmError::InvalidJumpTarget(target))
        } else {
            Ok(target as usize) // Convert valid address to usize index.
        }
    }

    // --- Execution Loop ---

    /// Runs the VM's fetch-decode-execute cycle until an `Exit` instruction
    /// is encountered or an error occurs.
    pub fn run(&mut self) -> Result<(), VmError> {
        self.running = true;
        while self.running {
            // Debugging aid (optional): Print VM state before each instruction.
            // println!(
            //     "PC:{:04X} SP:{:04X} BP:{:04X} AX:{:<11} | Next: {:?}",
            //     self.pc, self.sp, self.bp, self.ax,
            //     self.code.get(self.pc).and_then(|&v| Instruction::try_from(v).ok())
            // );

            let instruction = self.fetch_instruction()?; // Fetch and decode.
            self.execute(instruction)?;                // Execute.

            // Note: The `running` flag might be set to false within `execute` (by EXIT).
        }
        Ok(()) // Return Ok if EXIT was reached without errors.
    }

    /// Executes a single decoded `Instruction`.
    /// Modifies the VM state (registers, stack, pc) accordingly.
    #[inline] // Consider inlining if profiling suggests a benefit.
    fn execute(&mut self, instruction: Instruction) -> Result<(), VmError> {
        match instruction {
            Instruction::Nop => { /* Do nothing */ }
            Instruction::Imm => {
                 let operand = self.fetch_operand()?;
                 println!("[DEBUG] IMM: Setting AX to {}", operand); // DEBUG PRINT
                 self.ax = operand;
             }
            Instruction::Push => self.push(self.ax)?,
            Instruction::Exit => {
                self.running = false; // Signal the run loop to stop.
                println!("--- VM Exit --- Final AX: {} ---", self.ax); // DEBUG PRINT
            }

            // --- Arithmetic Operations ---
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

            // --- Control Flow ---
            Instruction::Jmp => {
                let target_addr = self.fetch_operand()?;
                self.pc = self.validate_jump_target(target_addr)?;
            }
            Instruction::Jz => {
                let target_addr = self.fetch_operand()?; // PC advanced here
                if self.ax == 0 {
                    // This line *should* overwrite the PC set by fetch_operand
                    self.pc = self.validate_jump_target(target_addr)?;
                }
                // If not taken, PC remains as set by fetch_operand - CORRECT
            }
            Instruction::Jnz => {
                let target_addr = self.fetch_operand()?; // PC advanced here
                 if self.ax != 0 {
                    // This line *should* overwrite the PC set by fetch_operand
                    self.pc = self.validate_jump_target(target_addr)?;
                 }
                 // If not taken, PC remains as set by fetch_operand - CORRECT
            }

            // --- Function Call Mechanism ---
            Instruction::Call => {
                let target_addr = self.fetch_operand()?; // pc advanced after operand
                println!("[DEBUG] CALL: Fetched target_addr={}, PC after fetch={}", target_addr, self.pc); // DEBUG PRINT
                let target_pc = self.validate_jump_target(target_addr)?;
                let return_addr = self.pc; // PC is currently position *after* operand
                println!("[DEBUG] CALL: Pushing return_addr={}", return_addr); // DEBUG PRINT
                self.push(return_addr as Value)?; // Pushes PC AFTER operand fetch
                println!("[DEBUG] CALL: Jumping to target_pc={}", target_pc); // DEBUG PRINT
                self.pc = target_pc;
            }
            Instruction::Ent => {
                let locals_size = self.fetch_operand()?;
                if locals_size < 0 { return Err(VmError::InvalidInstruction(locals_size)); }
                self.push(self.bp as Value)?;
                self.bp = self.sp;
                let new_sp = self.sp.checked_sub(locals_size as usize);
                match new_sp {
                    Some(sp) => self.sp = sp,
                    None => return Err(VmError::StackOverflow),
                }
                if self.sp > self.bp { return Err(VmError::StackOverflow); }
            }
            Instruction::Adj => {
                let arg_count = self.fetch_operand()?;
                if arg_count < 0 { return Err(VmError::InvalidInstruction(arg_count)); }
                let new_sp = self.sp.checked_add(arg_count as usize);
                match new_sp {
                    Some(sp) if sp <= STACK_TOP => self.sp = sp,
                    _ => return Err(VmError::StackUnderflow),
                }
            }
            Instruction::Lev => {
                println!("[DEBUG] LEV: Entering. SP={}, BP={}", self.sp, self.bp); // DEBUG PRINT
                self.sp = self.bp; // Restore SP
                println!("[DEBUG] LEV: SP set to BP. SP={}", self.sp); // DEBUG PRINT
                // Restore the caller's base pointer
                let popped_bp = self.pop()?;
                 if popped_bp < 0 || popped_bp as usize > STACK_TOP { println!("[DEBUG] LEV: Invalid popped BP!"); return Err(VmError::StackUnderflow); } // DEBUG PRINT
                self.bp = popped_bp as usize;
                 println!("[DEBUG] LEV: Popped BP={}. New BP={}, SP={}", popped_bp, self.bp, self.sp); // DEBUG PRINT

                // Restore the program counter by popping the return address.
                let return_pc_val = self.pop()?;
                println!("[DEBUG] LEV: Popped return_pc_val={}. SP={}", return_pc_val, self.sp); // DEBUG PRINT
                // Validate and set PC
                self.pc = self.validate_jump_target(return_pc_val)?;
                println!("[DEBUG] LEV: Set PC to {}. Exiting LEV.", self.pc); // DEBUG PRINT
            }

            // --- Memory Access Instructions ---
            Instruction::Li => {
                let addr = self.validate_memory_address(self.ax)?;
                self.ax = self.stack[addr];
            }
            Instruction::Lc => {
                let addr = self.validate_memory_address(self.ax)?;
                self.ax = self.stack[addr] & 0xFF;
            }
            Instruction::Si => {
                let value_to_store = self.pop()?;
                let addr = self.validate_memory_address(self.ax)?;
                self.stack[addr] = value_to_store;
            }
            Instruction::Sc => {
                let value_to_store = self.pop()?;
                let addr = self.validate_memory_address(self.ax)?;
                self.stack[addr] = value_to_store & 0xFF;
            }
        }
        Ok(()) // Instruction executed successfully.
    }
} // end impl VirtualMachine