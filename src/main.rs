mod lexer;
mod vm;
mod parser;
mod symbol;

use std::fs;
use std::path::Path;
use std::process::exit;
// Required for flushing stdout for printf
use std::io::{self, Write};


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <source.c>", args[0]);
        exit(1);
    }

    let source_path = Path::new(&args[1]);
    let source_code = match fs::read_to_string(source_path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error reading file {:?}: {}", source_path, e);
            exit(1);
        }
    };

    println!("--- Compiling {} ---", source_path.display());
    let lexer = lexer::Lexer::new(&source_code);
    let parser = parser::Parser::new(lexer);

    match parser.parse_program() {
        Ok((code, data, entry_point)) => { // Get code, data, entry point
            println!("Compilation successful!");
            println!("Entry point: {}", entry_point);
            println!("Generated Code ({} instructions).", code.len());
            println!("Data Segment ({} bytes).", data.len());
            // Optional: Print generated code/data for debugging
            // vm::VirtualMachine::dump_code(&code); // Need a static method for this
            // vm::VirtualMachine::dump_data_segment_static(&data); // Need a static method


            println!("\n--- Running ---");
            // Pass both code and data to the VM constructor
            let mut vm = vm::VirtualMachine::new(code, data);
            match vm.run() {
                Ok(result) => {
                    // Ensure output buffer is flushed, especially after printf without newline
                    io::stdout().flush().unwrap_or_else(|e| eprintln!("Warning: Failed to flush stdout: {}", e));
                    println!("\n--- Execution Finished ---");
                    println!("Exit code / Final AX value: {}", result);
                    exit(result); // Exit with the program's return code (already i32)
                }
                Err(vm_err) => {
                     // Ensure output buffer is flushed even on error
                    io::stdout().flush().unwrap_or_else(|e| eprintln!("Warning: Failed to flush stdout: {}", e));
                    eprintln!("\n--- Runtime Error ---");
                    eprintln!("{}", vm_err);
                    // Dump VM state on error for debugging
                    vm.dump_registers();
                    vm.dump_stack(16); // Show top 16 stack entries
                    // vm.dump_data_segment(); // Optionally dump data segment
                    exit(1);
                }
            }
        }
        Err(parse_err) => {
            eprintln!("\n--- Compilation Failed ---");
            eprintln!("{}", parse_err);
            exit(1);
        }
    }
}