// tests/integration_tests.rs

use c4_rust_team_shawkah::lexer::Lexer; // <-- Make sure this matches your crate name
use c4_rust_team_shawkah::parser::Parser;
use c4_rust_team_shawkah::vm::{VirtualMachine, Value, VmError}; // Import VmError

// Define a type alias for the result for cleaner test code
// Returns Ok(exit_code) or Err(error_message_string)
type RunResult = Result<Value, String>;

/// Helper function to run C code through the full compiler pipeline (Lexer -> Parser -> VM).
/// CANNOT capture or return stdout.
fn run_c_code(source: &str) -> RunResult {
    let lexer = Lexer::new(source);
    let parser = Parser::new(lexer);

    // 1. Parsing
    match parser.parse_program() {
        Ok((code, data_segment, _entry_point)) => {
            // 2. VM Setup - Handle the Result from new()
            // Use map_err to convert potential VmError into String for our RunResult
            // Use '?' to extract the VirtualMachine or propagate the error early.
            let mut vm = VirtualMachine::new(code, data_segment)
                .map_err(|init_err| format!("VM Initialization Error: {}", init_err))?; // FIXED HERE

            // Now 'vm' is guaranteed to be a VirtualMachine instance

            // 3. VM Execution
            match vm.run() {
                Ok(exit_code) => Ok(exit_code), // Only return the exit code
                Err(vm_err) => Err(format!("VM Runtime Error: {}", vm_err)),
            }
        }
        Err(parse_err) => Err(format!("Parse Error: {}", parse_err)),
    }
}

// --- Integration Tests ---

#[test]
fn test_simple_return() {
    let code = "int main() { return 42; }";
    match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 42, "Exit code mismatch");
        }
        Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
        }
    }
}

#[test]
fn test_basic_assignment_and_add() {
    let code = r#"
        int main() {
            int a;
            int b;
            a = 10;
            b = a + 5;
            return b;
        }
    "#;
    match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 15, "Exit code mismatch (expected 10 + 5)");
        }
         Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
         }
    }
}


#[test]
fn test_simple_printf() {
    // We can only test the exit code, not the printed output "Hello C4!"
    let code = r#"
        int main() {
            printf("Hello C4!"); // printf returns number of chars written (9)
            return 0;           // Explicit return 0
        }
    "#;
    match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 0, "Exit code mismatch");
            // We assume printf worked if the exit code is correct and no VM error occurred.
        }
         Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
         }
    }
}

#[test]
fn test_printf_int_arg() {
    let code = r#"
        int main() {
            int value;
            value = 123;
            printf("Number: %d.", value); // String is "Number: 123.", length 13
            return value - 23; // Should be 100
        }
    "#;
     match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 100, "Exit code mismatch (expected 100)");
        }
        Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
        }
    }
}

// Test unary minus, since your parser supports it via the SUB token rule in parse_primary
#[test]
fn test_printf_multiple_args() {
     let code = r#"
        int main() {
            int x;
            int y;
            x = 5;
            y = -10; // Uses unary minus
            printf("X=%d, Y=%d\n", x, y); // "X=5, Y=-10\n", length 12
            return x + y; // 5 + (-10) = -5
        }
    "#;
     match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, -5, "Exit code mismatch");
        }
        Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
        }
    }
}


#[test]
fn test_printf_no_args() {
    let code = r#"
        int main() {
            printf("Just text.\n"); // "Just text.\n", length 11
            return 55;
        }
    "#;
     match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 55, "Exit code mismatch");
        }
        Err(e) => {
            panic!("Test failed unexpectedly: {}", e);
        }
    }
}

// Test case similar to the successful execution log provided
#[test]
fn test_hello_world_like_example() {
    let code = r#"
        int main() {
            printf("Hello, World!\n"); // "Hello, World!\n", length 14
            return 0;
        }
    "#;
    match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 0, "Exit code mismatch for hello world");
        }
        Err(e) => {
            panic!("Test failed unexpectedly for hello world: {}", e);
        }
    }
}

// Test for the new ** operator
#[test]
fn test_exponentiation_operator() {
    let code = r#"
        int main() {
            int x = 2;
            int y = 3;
            int z = x ** y; // 2 ** 3 = 8
            return z;
        }
    "#;
    match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 8, "Exit code mismatch for 2 ** 3");
        }
        Err(e) => {
            panic!("Test failed unexpectedly for exponentiation: {}", e);
        }
    }
}

#[test]
fn test_exponentiation_complex() {
    let code = r#"
        int main() {
            int a = 3;
            int b = 2;
            return a ** (1 + b) + 1; // 3 ** (1 + 2) + 1 = 3 ** 3 + 1 = 27 + 1 = 28
        }
    "#;
     match run_c_code(code) {
        Ok(exit_code) => {
            assert_eq!(exit_code, 28, "Exit code mismatch for complex exponentiation");
        }
        Err(e) => {
            panic!("Test failed unexpectedly for complex exponentiation: {}", e);
        }
    }
}