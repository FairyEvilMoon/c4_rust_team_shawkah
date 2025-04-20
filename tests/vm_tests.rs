
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
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE); // Check initial SP
    assert_eq!(vm.bp, DEFAULT_MEM_SIZE); // Check initial BP
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

    assert_eq!(vm.ax, 10);
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE - 2); // Check SP position
    assert_eq!(vm.stack[vm.sp], 10); // Check value at SP
    assert_eq!(vm.stack[vm.sp + 1], 5); // Check value below SP
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
    // Calculate 3 + 4 = 7
    // Stack before ADD: [..., 3] <- sp
    // AX before ADD: 4
    // After ADD: AX = 7, sp moved past 3
    let code = vec![
        Instruction::Imm as Value, 3, Instruction::Push as Value, // ax = 3, push 3.
        Instruction::Imm as Value, 4,       // ax = 4
        Instruction::Add as Value,          // Pops 3, ax = 3 + 4 = 7
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM ADD execution failed");
    assert_eq!(vm.ax, 7);
    // SP should be back where it was before the PUSH
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE); // Check relative to initial SP
}

#[test]
fn test_sub() {
    // Calculate 10 - 4 = 6
    // Stack before SUB: [..., 10] <- sp
    // AX before SUB: 4
    // After SUB: AX = 6, sp moved past 10
    let code = vec![
        Instruction::Imm as Value, 10, Instruction::Push as Value, // ax = 10, push 10.
        Instruction::Imm as Value, 4,        // ax = 4
        Instruction::Sub as Value,           // Pops 10, ax = 10 - 4 = 6
        Instruction::Exit as Value,
    ];
    let vm = run_code(code).expect("VM SUB execution failed");
    assert_eq!(vm.ax, 6);
    // SP should be back where it was before the PUSH
    assert_eq!(vm.sp, DEFAULT_MEM_SIZE); // Check relative to initial SP
}

#[test]
fn test_add_stack_underflow() {
    // ADD requires one item on stack, but stack is empty
    let code = vec![Instruction::Imm as Value, 5, Instruction::Add as Value, Instruction::Exit as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::StackUnderflow));
}

#[test]
fn test_sub_stack_underflow() {
    // SUB requires one item on stack, but stack is empty
    let code = vec![Instruction::Imm as Value, 5, Instruction::Sub as Value, Instruction::Exit as Value];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::StackUnderflow));
}

#[test]
fn test_add_overflow() {
     let code = vec![
        Instruction::Imm as Value, i32::MAX, Instruction::Push as Value, // Push MAX
        Instruction::Imm as Value, 1, // ax = 1
        Instruction::Add as Value, // Attempt MAX + 1
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::ArithmeticOverflow));
}

#[test]
fn test_sub_overflow() { // Example for sub overflow (underflow really)
     let code = vec![
        Instruction::Imm as Value, i32::MIN, Instruction::Push as Value, // Push MIN
        Instruction::Imm as Value, 1, // ax = 1
        Instruction::Sub as Value, // Attempt MIN - 1
        Instruction::Exit as Value,
    ];
    let result = run_code(code);
    assert_eq!(result, Err(VmError::ArithmeticOverflow));
}