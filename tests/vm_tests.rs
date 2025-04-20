use c4_rust_team_shawkah::vm::*;
use c4_rust_team_shawkah::vm::{Value, DEFAULT_MEM_SIZE};

fn run_code(code: Vec<Value>) -> Result<VirtualMachine, VmError> {
    let mut vm = VirtualMachine::new(code);
    vm.run()?;
    Ok(vm)
}

#[test]
fn vm_init() {
    let vm = VirtualMachine::new(vec![]);
    assert_eq!(vm.pc, 0);
    assert_eq!(vm.ax, 0);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
    assert_eq!(vm.bp, DEFAULT_MEM_SIZE);
}

#[test]
fn vm_simple_exit() {
    let code = vec![Instruction::Exit as Value];
    let result = run_code(code);
    assert!(result.is_ok());
}

#[test]
fn test_imm_exit() {
    let code = vec![Instruction::Imm as Value, 123, Instruction::Exit as Value];
    let vm = run_code(code).expect("VM execution failed");
    assert_eq!(vm.ax, 123);
}

#[test]
fn test_imm_push_exit() {
    let code = vec![
        Instruction::Imm as Value, 5, Instruction::Push as Value,
        Instruction::Imm as Value, 10, Instruction::Push as Value,
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM execution failed");
    assert_eq!(vm.ax, 10);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE - 2);
    assert_eq!(vm.stack[vm.sp], 10);
    assert_eq!(vm.stack[vm.sp + 1], 5);
}

#[test]
fn test_stack_overflow_via_run() {
    let mut code = vec![Instruction::Imm as Value, 1];
    for _ in 0..(DEFAULT_MEM_SIZE + 5) {
        code.push(Instruction::Push as Value);
    }
    code.push(Instruction::Exit as Value);
    let mut vm = VirtualMachine::new(code);
    let result = vm.run();
    assert_eq!(result, Err(VmError::StackOverflow));
}


#[test]
fn test_pc_out_of_bounds_and_operand_expected() {
    let mut vm_operand = VirtualMachine::new(vec![Instruction::Imm as Value]);
    let result_operand = vm_operand.run();
    assert_eq!(result_operand, Err(VmError::OperandExpected));

    let mut vm_pc = VirtualMachine::new(vec![Instruction::Imm as Value, 5, Instruction::Push as Value]);
    let result_pc = vm_pc.run();
    assert_eq!(result_pc, Err(VmError::PcOutOfBounds));
}

#[test]
fn test_add() {
    let code = vec![
        Instruction::Imm as Value, 3, Instruction::Push as Value,
        Instruction::Imm as Value, 4,
        Instruction::Add as Value,
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM ADD execution failed");
    assert_eq!(vm.ax, 7);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
}

#[test]
fn test_sub() {
    let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value,
        Instruction::Imm as Value, 4,
        Instruction::Sub as Value,
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM SUB execution failed");
    assert_eq!(vm.ax, 6);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
}

#[test]
fn test_add_stack_underflow() {
    let code = vec![Instruction::Imm as Value, 5, Instruction::Add as Value, Instruction::Exit as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::StackUnderflow));
}

#[test]
fn test_sub_stack_underflow() {
    let code = vec![Instruction::Imm as Value, 5, Instruction::Sub as Value, Instruction::Exit as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::StackUnderflow));
}

#[test]
fn test_add_overflow() {
     let code = vec![
        Instruction::Imm as Value, i32::MAX, Instruction::Push as Value,
        Instruction::Imm as Value, 1,
        Instruction::Add as Value,
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::ArithmeticOverflow));
}

#[test]
fn test_sub_overflow() {
     let code = vec![
        Instruction::Imm as Value, i32::MIN, Instruction::Push as Value,
        Instruction::Imm as Value, 1,
        Instruction::Sub as Value,
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::ArithmeticOverflow));
}


#[test]
fn test_mul() {
    // Calculate 3 * 4 = 12
    let code = vec![
        Instruction::Imm as Value, 3, Instruction::Push as Value, // Push 3
        Instruction::Imm as Value, 4,       // ax = 4
        Instruction::Mul as Value,          // Pops 3, ax = 3 * 4 = 12
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM MUL execution failed");
    assert_eq!(vm.ax, 12);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
}

#[test]
fn test_div() {
    // Calculate 10 / 3 = 3
    let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value, // Push 10 (dividend)
        Instruction::Imm as Value, 3,        // ax = 3 (divisor)
        Instruction::Div as Value,           // Pops 10, ax = 10 / 3 = 3
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM DIV execution failed");
    assert_eq!(vm.ax, 3);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
}

#[test]
fn test_mod() {
    // Calculate 10 % 3 = 1
    let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value, // Push 10 (dividend)
        Instruction::Imm as Value, 3,        // ax = 3 (divisor)
        Instruction::Mod as Value,           // Pops 10, ax = 10 % 3 = 1
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM MOD execution failed");
    assert_eq!(vm.ax, 1);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE);
}

#[test]
fn test_div_by_zero() {
     let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value, // Push 10
        Instruction::Imm as Value, 0, // ax = 0 (divisor)
        Instruction::Div as Value, // Attempt 10 / 0
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::DivisionByZero));
}

#[test]
fn test_mod_by_zero() {
     let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value, // Push 10
        Instruction::Imm as Value, 0, // ax = 0 (divisor)
        Instruction::Mod as Value, // Attempt 10 % 0
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::DivisionByZero));
}

#[test]
fn test_mul_stack_underflow() {
    let code = vec![Instruction::Imm as Value, 5, Instruction::Mul as Value, Instruction::Exit as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::StackUnderflow));
}
#[test]
fn test_jmp() {
    let code = vec![
        Instruction::Jmp as Value, 5, // Jump to index 5
        Instruction::Imm as Value, 1, // Skipped
        Instruction::Nop as Value,    // Skipped
        Instruction::Exit as Value,   // Skipped
        Instruction::Imm as Value, 2, // Jump lands here
        Instruction::Nop as Value,    // Executed
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM JMP failed");
    assert_eq!(vm.ax, 2);
}

#[test]
fn test_jz_taken() {
    let code = vec![
        Instruction::Imm as Value, 0,  // AX = 0
        Instruction::Jz as Value, 7,   // Jump taken (target IMM 2)
        Instruction::Imm as Value, 1,  // Skipped
        Instruction::Nop as Value,     // Skipped
        Instruction::Exit as Value,    // Skipped
        Instruction::Imm as Value, 2,  // Jump lands here
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM JZ taken failed");
    assert_eq!(vm.ax, 2);
}

#[test]
fn test_jz_not_taken() {
    let code = vec![
        Instruction::Imm as Value, 1,  // AX = 1
        Instruction::Jz as Value, 7,   // Jump NOT taken
        Instruction::Imm as Value, 10, // Executed next
        Instruction::Exit as Value,    // Program ends here
        Instruction::Nop as Value,     // Skipped
        Instruction::Imm as Value, 2,  // Skipped
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM JZ not taken failed");
    assert_eq!(vm.ax, 10);
}

#[test]
fn test_jnz_taken() {
    let code = vec![
        Instruction::Imm as Value, 1,   // AX = 1
        Instruction::Jnz as Value, 7,  // Jump taken (target IMM 2)
        Instruction::Imm as Value, 1,   // Skipped
        Instruction::Nop as Value,      // Skipped
        Instruction::Exit as Value,     // Skipped
        Instruction::Imm as Value, 2,   // Jump lands here
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM JNZ taken failed");
    assert_eq!(vm.ax, 2);
}

#[test]
fn test_jnz_not_taken() {
     let code = vec![
        Instruction::Imm as Value, 0,   // AX = 0
        Instruction::Jnz as Value, 7,  // Jump NOT taken
        Instruction::Imm as Value, 10,  // Executed next
        Instruction::Exit as Value,     // Program ends here
        Instruction::Nop as Value,      // Skipped
        Instruction::Imm as Value, 2,   // Skipped
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM JNZ not taken failed");
    assert_eq!(vm.ax, 10);
}

#[test]
fn test_jump_operand_missing() {
    let code = vec![Instruction::Imm as Value, 0, Instruction::Jz as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::OperandExpected));
}

#[test]
fn test_jump_target_out_of_bounds() {
    let code = vec![Instruction::Jmp as Value, 100];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::InvalidJumpTarget(100)));

    let code2 = vec![Instruction::Jmp as Value, -1];
    let result2 = run_code(code2);
    assert_eq!(result2, Err(VmError::InvalidJumpTarget(-1)));

    let code3 = vec![Instruction::Jmp as Value, 2]; // Code size is 2
    let result3 = run_code(code3);
    assert_eq!(result3, Err(VmError::InvalidJumpTarget(2)));
}