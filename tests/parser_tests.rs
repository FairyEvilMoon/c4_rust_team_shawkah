// src/parser.rs
// ... (rest of the parser code) ...

#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::parser::{ParseError, Parser}; // Import Parser, ParseError, etc.
    use c4_rust_team_shawkah::lexer::{Lexer, Token}; // Need Lexer to feed the parser
    use c4_rust_team_shawkah::vm::{Instruction, Value}; // Need Instruction enum for assertions

    #[test]
    fn test_parse_hello_world() {
        let source = r#"
            // Simple hello world
            int main() {
                printf("Hello, World!\n"); // Use standard C escapes
                return 0;
            }
        "#;
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);
        let result = parser.parse_program();

        assert!(result.is_ok());
        let (code, data, entry_point) = result.unwrap();

        // 1. Check Entry Point
        assert_eq!(entry_point, 3, "Entry point should be after preamble"); // Preamble is CALL, ADDR, EXIT

        // 2. Check Preamble
        assert_eq!(code.get(0..3), Some(&[
            Instruction::Call as Value, // Call
            3,                          // Address of main (index 3)
            Instruction::Exit as Value, // Exit
        ][..]), "Preamble instructions mismatch");

        // 3. Check Data Segment
        let expected_data = b"Hello, World!\n\0"; // Note the null terminator added by parser
        assert_eq!(data, expected_data, "Data segment content mismatch");

        // 4. Check Code Structure (basic checks, not exhaustive byte matching)
        // Code for main starts at index 3
        assert_eq!(code.get(3), Some(&(Instruction::Ent as Value)), "Main should start with ENT");
        assert_eq!(code.get(4), Some(&0), "ENT operand should be 0"); // 0 locals

        // Find printf call sequence (Imm <addr>, Push, Call -1, Adj 1)
        let string_addr = expected_data.as_ptr() as usize - data.as_ptr() as usize; // Calculate expected addr
        assert!(code.contains(&(Instruction::Imm as Value)), "Should have IMM for string");
        // Find the specific Imm for the string address
        let imm_str_idx = code.iter().position(|&v| v == string_addr as Value);
        assert!(imm_str_idx.is_some(), "Cannot find IMM instruction for string address");
        let imm_str_idx = imm_str_idx.unwrap();
        assert_eq!(code[imm_str_idx -1], Instruction::Imm as Value, "Instruction before string address should be IMM");


        let push_idx = imm_str_idx + 1;
        assert_eq!(code.get(push_idx), Some(&(Instruction::Push as Value)), "Should PUSH string addr");

        let call_printf_idx = push_idx + 1;
        assert_eq!(code.get(call_printf_idx), Some(&(Instruction::Call as Value)), "Should CALL");
        assert_eq!(code.get(call_printf_idx + 1), Some(&-1), "Should CALL printf addr (-1)");

        let adj_idx = call_printf_idx + 2;
        assert_eq!(code.get(adj_idx), Some(&(Instruction::Adj as Value)), "Should ADJ stack");
        assert_eq!(code.get(adj_idx + 1), Some(&1), "Should ADJ by 1 arg");

        // Find return sequence (Imm 0, Lev)
        assert!(code.ends_with(&[Instruction::Imm as Value, 0, Instruction::Lev as Value]), "Main code should end with IMM 0, LEV");

        // Basic length check (adjust if exact length is known and stable)
        // Preamble(3) + ENT(2) + IMM str(2) + PUSH(1) + CALL printf(2) + ADJ(2) + IMM 0(2) + LEV(1) = 15
        assert_eq!(code.len(), 15, "Unexpected code length");
    }

    #[test]
    fn test_parse_missing_semicolon() {
        let source = "int main() { return 0 }"; // Missing semicolon
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);
        let result = parser.parse_program();
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Check the error type and potentially the location/message
        assert!(matches!(err, ParseError::UnexpectedToken { expected, found, .. }
            if expected == "';'" && found == Token::RBrace ));
    }

     #[test]
    fn test_parse_wrong_function_name() {
        let source = "int wrong() { return 0; }";
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);
        let result = parser.parse_program();
        assert!(result.is_err());
        let err = result.unwrap_err();
         assert!(matches!(err, ParseError::Other(msg) if msg.contains("Expected 'main' function")));
    }

     #[test]
    fn test_parse_unexpected_token_in_body() {
        let source = "int main() { int x = 0; return 0; }"; // Contains 'int' which parser doesn't handle yet
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);
        let result = parser.parse_program();
        assert!(result.is_err());
        // The error depends on where the simplified expression parser fails.
        // It likely fails expecting an expression after '{', but finds 'int'.
        assert!(matches!(result.unwrap_err(), ParseError::UnexpectedToken { found: Token::Int, .. }));
    }

     #[test]
    fn test_parse_extra_tokens_after_main() {
        let source = "int main() { return 0; } int foo;";
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);
        let result = parser.parse_program();
        assert!(result.is_err());
        // Should fail because 'int' is found after main's '}' when EOF was expected
        assert!(matches!(result.unwrap_err(), ParseError::UnexpectedToken { expected, found: Token::Int, .. } if expected == "end of file"));
    }
}