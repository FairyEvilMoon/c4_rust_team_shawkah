use c4_rust_team_shawkah::vm::*;
use c4_rust_team_shawkah::vm::Value; 

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
    assert!(vm.sp > 0);
    assert_eq!(vm.sp, vm.bp);
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
        Instruction::Imm as Value, 5, Instruction::Push as Value, // ax = 5, push 5
        Instruction::Imm as Value, 10, Instruction::Push as Value, // ax = 10, push 10
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM execution failed");

    // Stack grows down, sp points to the last pushed item's location
    assert_eq!(vm.ax, 10); // ax holds the last immediate value
    assert_eq!(vm.stack[vm.sp], 10); // sp points here after last push
    assert_eq!(vm.stack[vm.sp + 1], 5);
    // Check SP position relative to end (assuming default mem size for simplicity)
    // This is fragile, better to check relative stack content.
    // assert_eq!(vm.sp, DEFAULT_MEM_SIZE - 2); // Fragile check
}

#[test]
fn test_stack_overflow_via_run() {
    // Create enough push instructions to potentially overflow
    let mut code = vec![Instruction::Imm as Value, 1]; // IMM 1
    for _ in 0..(DEFAULT_MEM_SIZE + 5) { // Try to push more than stack size
        code.push(Instruction::Push as Value);
    }
    code.push(Instruction::Exit as Value); // Add exit at the end

    let mut vm = VirtualMachine::new(code);
    let result = vm.run();
    // Expect StackOverflow during the run loop
    assert_eq!(result, Err(VmError::StackOverflow));
}


#[test]
fn test_pc_out_of_bounds_and_operand_expected() {
    // Code has IMM but no operand value following it
    let mut vm_operand = VirtualMachine::new(vec![Instruction::Imm as Value]);
    let result_operand = vm_operand.run();
    assert_eq!(result_operand, Err(VmError::OperandExpected));

    // Code runs off the end after valid instructions (no EXIT)
    let mut vm_pc = VirtualMachine::new(vec![Instruction::Imm as Value, 5, Instruction::Push as Value]);
    let result_pc = vm_pc.run();
    // Should fail when trying to fetch instruction after PUSH
    assert_eq!(result_pc, Err(VmError::PcOutOfBounds));
}