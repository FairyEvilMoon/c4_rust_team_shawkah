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