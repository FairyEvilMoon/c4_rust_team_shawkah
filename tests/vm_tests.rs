// src/vm.rs
// ... (rest of the vm code) ...

#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::vm::{Instruction, Value, VirtualMachine, VmError, DEFAULT_MEM_SIZE};

    // Helper function to create and run VM with simple code
    fn run_vm_code(code: Vec<Value>) -> Result<Value, VmError> {
        // Use empty data segment for most tests
        let data: Vec<u8> = Vec::new();
        let mut vm = VirtualMachine::new(code, data);
        vm.run()
    }

    #[test]
    fn test_imm_exit() {
        let code = vec![
            Instruction::Imm as Value, 42,
            Instruction::Exit as Value,
        ];
        assert_eq!(run_vm_code(code), Ok(42));
    }

    #[test]
    fn test_push_add_exit() {
        let code = vec![
            Instruction::Imm as Value, 10,
            Instruction::Push as Value,        // SP--, Mem[SP] = 10
            Instruction::Imm as Value, 5,      // AX = 5
            Instruction::Add as Value,         // AX = Pop() + AX = 10 + 5 = 15
            Instruction::Exit as Value,
        ];
        assert_eq!(run_vm_code(code), Ok(15));
    }

     #[test]
    fn test_push_sub_exit() {
        let code = vec![
            Instruction::Imm as Value, 3,      // AX=3
            Instruction::Push as Value,        // SP--, Mem[SP] = 3
            Instruction::Imm as Value, 8,      // AX = 8
            Instruction::Sub as Value,         // AX = Pop() - AX = 3 - 8 = -5
            Instruction::Exit as Value,
        ];
        assert_eq!(run_vm_code(code), Ok(-5));
    }

    #[test]
    fn test_basic_jump() {
        let code = vec![
            Instruction::Jmp as Value, 3,      // PC = 3
            Instruction::Imm as Value, 1,      // Skipped
            Instruction::Exit as Value,        // Skipped
            Instruction::Imm as Value, 99,     // Jump target
            Instruction::Exit as Value,
        ];
         assert_eq!(run_vm_code(code), Ok(99));
    }

    #[test]
    fn test_jz_taken() {
        let code = vec![
            Instruction::Imm as Value, 0,      // AX = 0
            Instruction::Jz as Value, 5,       // Jump taken (PC=5)
            Instruction::Imm as Value, 1,      // Skipped
            Instruction::Exit as Value,        // Skipped
            Instruction::Imm as Value, 100,    // Jump target
            Instruction::Exit as Value,
        ];
         assert_eq!(run_vm_code(code), Ok(100));
    }

    #[test]
    fn test_jz_not_taken() {
        let code = vec![
            Instruction::Imm as Value, 1,      // AX = 1
            Instruction::Jz as Value, 5,       // Jump not taken
            Instruction::Imm as Value, 200,    // Executed
            Instruction::Exit as Value,        // Executed
            Instruction::Imm as Value, 1,      // Skipped
            Instruction::Exit as Value,        // Skipped
        ];
         assert_eq!(run_vm_code(code), Ok(200));
    }

    #[test]
    fn test_ent_lev_simple() {
         // Call a simple function that just returns a value
         let code = vec![
             // Preamble-like setup
             Instruction::Call as Value, 3,  // Call function at index 3
             Instruction::Exit as Value,     // Exit after call returns

             // Function body (starts at index 3)
             Instruction::Ent as Value, 0,  // Enter function, 0 locals
             Instruction::Imm as Value, 55, // Load return value
             Instruction::Lev as Value,     // Leave function (restores BP, pops PC to return addr)
         ];
         assert_eq!(run_vm_code(code), Ok(55)); // Check final AX value
    }

    #[test]
    fn test_call_adj() {
        // Call function, check stack adjustment
        let code = vec![
            // Main code
            Instruction::Imm as Value, 11,
            Instruction::Push as Value,      // Push arg 1
            Instruction::Imm as Value, 22,
            Instruction::Push as Value,      // Push arg 2
            Instruction::Call as Value, 9,   // Call func at index 9
            Instruction::Adj as Value, 2,    // Remove 2 args from stack
            Instruction::Exit as Value,      // Exit (AX should hold func return value)

            // Function body (index 9)
            Instruction::Ent as Value, 0,   // 0 locals
            Instruction::Imm as Value, 99,  // Func return value
            Instruction::Lev as Value,      // Leave
        ];

        // Check final result (AX) and that stack is cleaned up (no underflow on Exit)
        let data: Vec<u8> = Vec::new();
        let mut vm = VirtualMachine::new(code, data);
        let result = vm.run();
        assert_eq!(result, Ok(99));
        // Check if SP is back where it started before the args were pushed
        // We start with SP = memory_size. Pushing twice makes it memory_size - 2.
        // Calling pushes PC (memory_size - 3). ENT pushes BP (memory_size - 4), sets BP = memory_size - 4.
        // LEV sets SP = BP = memory_size - 4, pops BP, pops PC. SP = memory_size - 2.
        // ADJ 2 adds 2 to SP. SP = memory_size.
        assert_eq!(vm.sp, vm.memory_size, "Stack pointer not restored after ADJ");
    }

    #[test]
    fn test_printf_hook_simple() {
        // Test the CALL -1 hook
        // NOTE: This test CANNOT verify the actual printed output easily.
        // It primarily checks that the VM executes the hook without crashing
        // and potentially returns the correct length in AX.

        let test_string = "Test\n";
        let mut data = Vec::from(test_string.as_bytes());
        data.push(0); // Null terminate

        let string_addr: Value = 0; // String starts at address 0 in data segment

        let code = vec![
            Instruction::Imm as Value, string_addr, // Load string address
            Instruction::Push as Value,           // Push address onto stack
            Instruction::Call as Value, -1,       // Call printf hook
            Instruction::Exit as Value,
        ];

        let mut vm = VirtualMachine::new(code, data);
        let result = vm.run();

        // Check that it ran without error and AX has the string length
        assert_eq!(result, Ok(test_string.len() as Value));
    }

    #[test]
    fn test_stack_overflow() {
        // Push repeatedly until overflow
        let mut code = vec![Instruction::Imm as Value, 1]; // Need something in AX to push
        // Use DEFAULT_MEM_SIZE to estimate stack capacity
        for _ in 0..(DEFAULT_MEM_SIZE / std::mem::size_of::<Value>() + 5) { // Push more than capacity
            code.push(Instruction::Push as Value);
        }
        code.push(Instruction::Exit as Value); // Should not be reached

        let result = run_vm_code(code);
        assert_eq!(result, Err(VmError::StackOverflow));
    }

     #[test]
     fn test_div_by_zero() {
         let code = vec![
             Instruction::Imm as Value, 0,      // AX = 0 (divisor)
             Instruction::Push as Value,
             Instruction::Imm as Value, 10,     // AX = 10 (dividend)
             Instruction::Div as Value,         // 10 / 0 -> Error
             Instruction::Exit as Value,
         ];
         let result = run_vm_code(code);
         assert!(matches!(result, Err(VmError::ArithmeticError(ref msg)) if msg == "Division by zero"));
     }

     #[test]
     fn test_memory_access_li_si() {
         // Store a value and load it back
         let code = vec![
             Instruction::Imm as Value, 100,    // Address to use (word address)
             Instruction::Push as Value,       // Push address for SI
             Instruction::Imm as Value, 12345,  // Value to store in AX
             Instruction::Si as Value,         // Store AX (12345) into Mem[Pop()] (Mem[100])
             Instruction::Imm as Value, 100,    // Load address 100 into AX
             Instruction::Li as Value,         // Load AX from Mem[AX] (Mem[100])
             Instruction::Exit as Value,
         ];
         assert_eq!(run_vm_code(code), Ok(12345));
     }

     #[test]
     fn test_memory_access_out_of_bounds() {
         let bad_addr = (DEFAULT_MEM_SIZE / std::mem::size_of::<Value>()) as Value; // Just beyond memory
          let code = vec![
             Instruction::Imm as Value, bad_addr, // Load bad address into AX
             Instruction::Li as Value,         // Try to load from Mem[AX] -> Error
             Instruction::Exit as Value,
         ];
         let result = run_vm_code(code);
         assert!(matches!(result, Err(VmError::MemoryAccessOutOfBounds { address, memory_type })
            if address == bad_addr && memory_type == "Stack/Heap"));
     }
}