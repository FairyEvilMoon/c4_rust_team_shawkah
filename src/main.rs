use std::{env, fs, process};

// Adjust path based on your project structure (e.g., `c4_rust_compiler::*`)
// Assuming your crate name in Cargo.toml is c4_rust_team_shawkah
use c4_rust_team_shawkah::lexer::Lexer;
use c4_rust_team_shawkah::parser::Parser;
use c4_rust_team_shawkah::vm::{VirtualMachine, VmError}; // Import VmError

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_c_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let source_code = match fs::read_to_string(filename) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            process::exit(1);
        }
    };

    // 1. Lexing
    let lexer = Lexer::new(&source_code);

    // 2. Parsing
    let parser = Parser::new(lexer);
    let (code, data_segment, entry_point) = match parser.parse_program() {
        Ok((code, data, entry)) => (code, data, entry),
        Err(e) => {
            eprintln!("Parse Error: {}", e);
            // Optional: Print line/column info if ParseError includes it
            process::exit(1);
        }
    };

    println!("--- Compilation Successful ---");
    println!("Entry Point Address (word index): {}", entry_point); // Assuming entry_point is word index


    // 3. VM Execution - Handle potential initialization error
    let mut vm = match VirtualMachine::new(code, data_segment) {
        Ok(initialized_vm) => initialized_vm,
        Err(init_err) => {
            eprintln!("\n--- VM Initialization Error ---");
            eprintln!("{}", init_err);
            process::exit(1); // Exit if VM can't be created
        }
    };

    println!("--- Running VM ---");

    match vm.run() { // Now calling run() on the actual VirtualMachine instance
        Ok(result) => {
            // The "result" here is the value in AX when the program exits (usually the return code of main).
            // The actual output (from printf) happens during execution via print! in the VM.
            println!("\n--- VM Finished ---");
            println!("Exit code (AX): {}", result);
            process::exit(result); // Exit with the program's return code
        }
        Err(e) => {
            eprintln!("\n--- VM Runtime Error ---");
            eprintln!("{}", e);
            vm.dump_registers();
            vm.dump_stack(20); // Show more stack on error
            process::exit(1);
        }
    }
}