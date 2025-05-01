#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::vm::{Instruction, VirtualMachine, VmError, Value, STACK_TOP, DEFAULT_MEM_SIZE};

    // Helper to create and run VM, returning final state or error
    fn run_code(code: Vec<Value>) -> Result<VirtualMachine, VmError> {
        let mut vm = VirtualMachine::new(code);
        vm.run()?;
        Ok(vm)
    }

    // Helper to create and run VM, expecting success and returning final AX
     fn run_code_get_ax(code: Vec<Value>) -> Value {
        let vm = run_code(code).expect("VM execution failed unexpectedly");
        vm.ax
    }

     // Helper to create and run VM, expecting specific error
    fn run_code_expect_error(code: Vec<Value>, expected_error: VmError) {
        let result = run_code(code);
        assert!(result.is_err(), "Expected VM error {:?}, but got Ok({:?})", expected_error, result.as_ref().ok());
        let actual_error = result.unwrap_err();
        assert_eq!(actual_error, expected_error, "Expected VM error: {:?}\n           but got: {:?}", expected_error, actual_error);
    }


    #[test]
    fn test_nop() {
        let code = vec![Instruction::Nop as Value, Instruction::Exit as Value];
        let vm = run_code(code).unwrap();
        assert_eq!(vm.pc, 2, "PC should be 2 after fetching NOP and EXIT");
        assert_eq!(vm.ax, 0);
    }

    #[test]
    fn test_imm_push() {
        let code = vec![
            Instruction::Imm as Value, 42,
            Instruction::Push as Value,
            Instruction::Imm as Value, 99,
            Instruction::Exit as Value,
        ];
        let vm = run_code(code).unwrap();
        assert_eq!(vm.ax, 99);
        assert_eq!(vm.sp, STACK_TOP - 1);
        assert_eq!(vm.stack[vm.sp], 42);
        assert_eq!(vm.pc, 6, "PC should be 6 after fetching EXIT");
    }

    #[test]
    fn test_exit() {
         let code = vec![
             Instruction::Imm as Value, 123,
             Instruction::Exit as Value,
             Instruction::Imm as Value, 999 // Skipped
         ];
         let vm = run_code(code).unwrap();
         assert_eq!(vm.ax, 123);
         assert_eq!(vm.pc, 3, "PC stops after fetching EXIT");
    }

    // --- Arithmetic Tests (PC should end after EXIT) ---
    #[test]
    fn test_add() {
        let code = vec![ Instruction::Imm as Value, 10, Instruction::Push as Value, Instruction::Imm as Value, 5, Instruction::Add as Value, Instruction::Exit as Value, ];
        assert_eq!(run_code_get_ax(code.clone()), 15); // Clone here
        let vm=run_code(code).unwrap(); assert_eq!(vm.pc, 7);
    }
    #[test]
    fn test_sub() {
        let code = vec![ Instruction::Imm as Value, 20, Instruction::Push as Value, Instruction::Imm as Value, 7, Instruction::Sub as Value, Instruction::Exit as Value, ];
        assert_eq!(run_code_get_ax(code.clone()), 13); // Clone here
        let vm=run_code(code).unwrap(); assert_eq!(vm.pc, 7);
    }
    #[test]
    fn test_mul() {
        let code = vec![ Instruction::Imm as Value, 6, Instruction::Push as Value, Instruction::Imm as Value, 7, Instruction::Mul as Value, Instruction::Exit as Value, ];
        assert_eq!(run_code_get_ax(code.clone()), 42); // Clone here
        let vm=run_code(code).unwrap(); assert_eq!(vm.pc, 7);
    }
    #[test]
    fn test_div() {
        let code = vec![ Instruction::Imm as Value, 21, Instruction::Push as Value, Instruction::Imm as Value, 4, Instruction::Div as Value, Instruction::Exit as Value, ];
        assert_eq!(run_code_get_ax(code.clone()), 5); // Clone here
        let vm=run_code(code).unwrap(); assert_eq!(vm.pc, 7);
    }
    #[test]
    fn test_div_by_zero() {
        let code = vec![ Instruction::Imm as Value, 10, Instruction::Push as Value, Instruction::Imm as Value, 0, Instruction::Div as Value, Instruction::Exit as Value, ];
        run_code_expect_error(code, VmError::DivisionByZero);
    }
    #[test]
    fn test_mod() {
        let code = vec![ Instruction::Imm as Value, 23, Instruction::Push as Value, Instruction::Imm as Value, 5, Instruction::Mod as Value, Instruction::Exit as Value, ];
        assert_eq!(run_code_get_ax(code.clone()), 3); // Clone here
        let vm=run_code(code).unwrap(); assert_eq!(vm.pc, 7);
    }
    #[test]
    fn test_mod_by_zero() {
        let code = vec![ Instruction::Imm as Value, 10, Instruction::Push as Value, Instruction::Imm as Value, 0, Instruction::Mod as Value, Instruction::Exit as Value, ];
        run_code_expect_error(code, VmError::DivisionByZero);
    }
    #[test]
    fn test_arithmetic_stack_underflow() {
        let code = vec![ Instruction::Imm as Value, 5, Instruction::Add as Value, Instruction::Exit as Value, ];
        run_code_expect_error(code, VmError::StackUnderflow);
    }


    // --- Control Flow Tests ---
    #[test]
    fn test_jmp() {
        let code = vec![
            Instruction::Imm as Value, 1,    // 0, 1
            Instruction::Jmp as Value, 4,    // 2, 3 (target = index 4)
            Instruction::Imm as Value, 99,   // 3 (skipped instruction itself - JMP operand is at index 3)
            Instruction::Exit as Value,      // 4 (target)
        ];
       let vm = run_code(code).unwrap();
       assert_eq!(vm.ax, 1, "AX should be 1 from before JMP");
       assert_eq!(vm.pc, 5, "PC should be 5 after fetching EXIT");
    }

    #[test]
    fn test_jz_taken() {
         let code = vec![
            Instruction::Imm as Value, 0,  // 0, 1
            Instruction::Jz as Value, 5,   // 2, 3 (target = index 5, EXIT)
            Instruction::Imm as Value, 99, // 4 (skipped instruction)
            Instruction::Exit as Value,   // 5 (target)
        ];
       let vm = run_code(code).unwrap();
       assert_eq!(vm.ax, 0, "AX should remain 0 when jump is taken");
       assert_eq!(vm.pc, 6, "PC should be 6 after fetching EXIT");
    }

    #[test]
    fn test_jz_not_taken() {
         let code = vec![
            Instruction::Imm as Value, 1,  // 0, 1
            Instruction::Jz as Value, 5,   // 2, 3 (target = index 5, EXIT)
            Instruction::Imm as Value, 99, // 4 (executed)
            Instruction::Exit as Value,   // 5 (target, executed next)
        ];
       let vm = run_code(code).unwrap();
       assert_eq!(vm.ax, 99, "AX should be 99 when jump is not taken");
       assert_eq!(vm.pc, 7, "PC should be 7 after fetching EXIT");
    }

    #[test]
    fn test_jnz_taken() {
         let code = vec![
            Instruction::Imm as Value, 1,  // 0, 1
            Instruction::Jnz as Value, 5,  // 2, 3 (target = index 5, EXIT)
            Instruction::Imm as Value, 99, // 4 (skipped)
            Instruction::Exit as Value,   // 5 (target)
        ];
       let vm = run_code(code).unwrap();
       assert_eq!(vm.ax, 1, "AX should remain 1 when jump is taken");
       assert_eq!(vm.pc, 6, "PC should be 6 after fetching EXIT");
    }

    #[test]
    fn test_jnz_not_taken() {
         let code = vec![
            Instruction::Imm as Value, 0,  // 0, 1
            Instruction::Jnz as Value, 5,  // 2, 3 (target = index 5, EXIT)
            Instruction::Imm as Value, 99, // 4 (executed)
            Instruction::Exit as Value,   // 5 (target, executed next)
        ];
       let vm = run_code(code).unwrap();
       assert_eq!(vm.ax, 99, "AX should be 99 when jump is not taken");
       assert_eq!(vm.pc, 7, "PC should be 7 after fetching EXIT");
    }

    #[test] fn test_jump_out_of_bounds() { let code_small = vec![Instruction::Jmp as Value, 2]; run_code_expect_error(code_small, VmError::InvalidJumpTarget(2)); let code_large_target = vec![Instruction::Jmp as Value, 100]; run_code_expect_error(code_large_target, VmError::InvalidJumpTarget(100)); }
    #[test] fn test_jump_negative() { let code = vec![Instruction::Jmp as Value, -1]; run_code_expect_error(code, VmError::InvalidJumpTarget(-1)); }

    // --- Function Call Tests ---
    #[test]
    fn test_simple_call_lev() {
        let code = vec![
            /* 0 */ Instruction::Imm as Value, 1,    // Main code
            /* 2 */ Instruction::Call as Value, 5,   // Target func at index 5
            /* 4 */ Instruction::Imm as Value, 99,   // Return point
            /* 5 */ Instruction::Imm as Value, 42,   // Function code
            /* 7 */ Instruction::Lev as Value,
            /* 8 */ Instruction::Exit as Value,      // Added Exit after return path
        ];
        let vm = run_code(code).unwrap();
        assert_eq!(vm.ax, 99, "Final value after return should be 99");
        assert_eq!(vm.pc, 9, "PC should be 9 after fetching final EXIT");
        assert_eq!(vm.sp, STACK_TOP, "Stack should be clean");
        assert_eq!(vm.bp, STACK_TOP, "BP should be restored");
    }

    #[test]
    fn test_call_ent_adj_lev() {
        let func_addr = 11; // Start func immediately after main block (main needs 11 instructions/operands)
        let code = vec![
            // --- Main (indices 0-10) ---
            /* 0 */ Instruction::Imm as Value, 10,
            /* 2 */ Instruction::Push as Value,
            /* 3 */ Instruction::Imm as Value, 20,
            /* 5 */ Instruction::Push as Value,
            /* 6 */ Instruction::Call as Value, func_addr as Value, // Calls func at 11, pushes return addr 8
            /* 8 */ Instruction::Adj as Value, 2, // Return point, adjusts SP
            /*10 */ Instruction::Exit as Value,
            // --- Function (indices 11-15) ---
            /*11 */ Instruction::Ent as Value, 1, // Func entry, operand is size 1
            /*13 */ Instruction::Imm as Value, 123,
            /*15 */ Instruction::Lev as Value,
        ]; // Total size 16

        let vm = run_code(code).unwrap();

        assert_eq!(vm.ax, 123, "Function should return 123 in AX");
        assert_eq!(vm.pc, 11, "PC should be 11 after fetching EXIT");
        assert_eq!(vm.sp, STACK_TOP, "Stack pointer should be TOP after ADJ");
        assert_eq!(vm.bp, STACK_TOP, "Base pointer not restored correctly after LEV");
    }


    // --- Memory Access Tests ---
    const TEST_ADDR_INT: Value = STACK_TOP as Value - 10;
    const TEST_ADDR_CHAR: Value = STACK_TOP as Value - 11;
    #[test] fn test_si_li() { let code = vec![ Instruction::Imm as Value, 12345, Instruction::Push as Value, Instruction::Imm as Value, TEST_ADDR_INT, Instruction::Si as Value, Instruction::Imm as Value, TEST_ADDR_INT, Instruction::Li as Value, Instruction::Exit as Value, ]; let vm = run_code(code).unwrap(); assert_eq!(vm.ax, 12345); assert_eq!(vm.stack[TEST_ADDR_INT as usize], 12345); assert_eq!(vm.sp, STACK_TOP); }
    #[test] fn test_sc_lc() { let value_to_store = 0xABCD; let expected_char_val = 0xCD; let code = vec![ Instruction::Imm as Value, value_to_store, Instruction::Push as Value, Instruction::Imm as Value, TEST_ADDR_CHAR, Instruction::Sc as Value, Instruction::Imm as Value, 0, Instruction::Imm as Value, TEST_ADDR_CHAR, Instruction::Lc as Value, Instruction::Exit as Value, ]; let vm = run_code(code).unwrap(); assert_eq!(vm.ax, expected_char_val); assert_eq!(vm.stack[TEST_ADDR_CHAR as usize], expected_char_val); assert_eq!(vm.sp, STACK_TOP); }
    #[test] fn test_li_out_of_bounds() { let invalid_addr = DEFAULT_MEM_SIZE as Value; let code = vec![ Instruction::Imm as Value, invalid_addr, Instruction::Li as Value, Instruction::Exit as Value, ]; run_code_expect_error(code, VmError::MemoryAccessOutOfBounds(invalid_addr)); let negative_addr: Value = -1; let code_neg = vec![ Instruction::Imm as Value, negative_addr, Instruction::Li as Value, Instruction::Exit as Value, ]; run_code_expect_error(code_neg, VmError::MemoryAccessOutOfBounds(negative_addr)); }
    #[test] fn test_si_out_of_bounds() { let invalid_addr = DEFAULT_MEM_SIZE as Value; let code = vec![ Instruction::Imm as Value, 99, Instruction::Push as Value, Instruction::Imm as Value, invalid_addr, Instruction::Si as Value, Instruction::Exit as Value, ]; run_code_expect_error(code, VmError::MemoryAccessOutOfBounds(invalid_addr)); let negative_addr: Value = -1; let code_neg = vec![ Instruction::Imm as Value, 99, Instruction::Push as Value, Instruction::Imm as Value, negative_addr, Instruction::Si as Value, Instruction::Exit as Value, ]; run_code_expect_error(code_neg, VmError::MemoryAccessOutOfBounds(negative_addr)); }
    #[test] fn test_si_stack_underflow() { let code = vec![ Instruction::Imm as Value, TEST_ADDR_INT, Instruction::Si as Value, Instruction::Exit as Value, ]; run_code_expect_error(code, VmError::StackUnderflow); }

    // --- General Error Tests ---
    #[test]
    fn test_pc_out_of_bounds() {
        let code_falls_off = vec![Instruction::Nop as Value];
        run_code_expect_error(code_falls_off, VmError::PcOutOfBounds);
    }
    #[test] fn test_operand_expected() { let code = vec![Instruction::Imm as Value]; run_code_expect_error(code, VmError::OperandExpected); }
    #[test] fn test_invalid_instruction() { let invalid_opcode = 99; let code = vec![invalid_opcode, Instruction::Exit as Value]; run_code_expect_error(code, VmError::InvalidInstruction(invalid_opcode)); }
    #[test] fn test_stack_overflow() { let mut code = Vec::new(); for i in 0..=DEFAULT_MEM_SIZE { code.extend(&[Instruction::Imm as Value, i as Value, Instruction::Push as Value]); } code.push(Instruction::Exit as Value); run_code_expect_error(code, VmError::StackOverflow); }
    #[test] fn test_explicit_stack_underflow() { let code_add = vec![ Instruction::Add as Value, Instruction::Exit as Value, ]; run_code_expect_error(code_add, VmError::StackUnderflow); let code_si = vec![ Instruction::Imm as Value, TEST_ADDR_INT, Instruction::Si as Value, Instruction::Exit as Value, ]; run_code_expect_error(code_si, VmError::StackUnderflow); }

} // end mod tests