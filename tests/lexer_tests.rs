#[cfg(test)]
mod tests {
    // Adjust path if needed, e.g., TEST
    use c4_rust_team_shawkah::lexer::{Lexer, Token, LexerError};

    // Helper function to collect all tokens or the first error
    fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?;
            let is_eof = token == Token::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    #[test]
    fn test_empty_input() {
        assert_eq!(tokenize(""), Ok(vec![Token::Eof]));
    }

    #[test]
    fn test_only_whitespace() {
        assert_eq!(tokenize("  \n\t  "), Ok(vec![Token::Eof]));
    }

    #[test]
    fn test_keywords() {
        let input = "enum if else int char return sizeof while";
        let expected = vec![
            Token::Enum, Token::If, Token::Else, Token::Int, Token::Char,
            Token::Return, Token::Sizeof, Token::While, Token::Eof,
        ];
        assert_eq!(tokenize(input), Ok(expected));
    }

     #[test]
     fn test_identifiers() {
         let input = "myVar _var anIdentifier123 i";
         let expected = vec![
             Token::Ident("myVar".to_string()),
             Token::Ident("_var".to_string()),
             Token::Ident("anIdentifier123".to_string()),
             Token::Ident("i".to_string()),
             Token::Eof,
         ];
         assert_eq!(tokenize(input), Ok(expected));
     }

    #[test]
    fn test_numbers() {
        let input = "123 0 9876543210 42";
        let expected = vec![
            Token::Number(123), Token::Number(0), Token::Number(9876543210), Token::Number(42), Token::Eof,
        ];
        assert_eq!(tokenize(input), Ok(expected));
    }

     #[test]
     fn test_string_literals() {
         let input = r#" "hello" "" "with\nescape" "and \"quotes\" \0" "#; // Added null term escape
         let expected = vec![
             Token::StringLiteral("hello".to_string()),
             Token::StringLiteral("".to_string()),
             Token::StringLiteral("with\nescape".to_string()),
             Token::StringLiteral("and \"quotes\" \0".to_string()),
             Token::Eof,
         ];
         assert_eq!(tokenize(input), Ok(expected));
     }

     #[test]
     fn test_char_literals() {
         let input = r#" 'a' ' ' '\n' '\\' '\'' '\0' "#;
         let expected = vec![
             Token::CharLiteral('a'),
             Token::CharLiteral(' '),
             Token::CharLiteral('\n'),
             Token::CharLiteral('\\'),
             Token::CharLiteral('\''),
             Token::CharLiteral('\0'),
             Token::Eof,
         ];
         assert_eq!(tokenize(input), Ok(expected));
     }

    #[test]
    fn test_single_ambiguous_symbols() {
        let input = "; , ( ) { } [ ] * &";
        let expected = vec![
            Token::Semicolon, Token::Comma, Token::LParen, Token::RParen, Token::LBrace,
            Token::RBrace, Token::LBracket, Token::RBracket, Token::Asterisk, Token::Ampersand,
            Token::Eof,
        ];
        assert_eq!(tokenize(input), Ok(expected));
    }

    #[test]
    fn test_operators() {
        // Includes multi-char and single-char operators mixed
        let input = "= == != < > <= >= + - * / % ++ -- | ^ & ! || && << >>";
        let expected = vec![
             Token::Assign, Token::Eq, Token::Ne, Token::Lt, Token::Gt, Token::Le, Token::Ge,
             Token::Add, Token::Sub, Token::Asterisk, Token::Div, Token::Mod, Token::Inc, Token::Dec,
             Token::BitOr, Token::BitXor, Token::Ampersand, Token::Not, Token::LogOr, Token::LogAnd,
             Token::Shl, Token::Shr,
            Token::Eof,
        ];
        assert_eq!(tokenize(input), Ok(expected));
    }

     #[test]
     fn test_comments() {
         let input = r#"
             int x = 10; // This is a line comment
             char y = 'c'; /* This is a
                              block comment */
             return x; // Another comment
             /* block at end */
         "#;
         let expected = vec![
             Token::Int, Token::Ident("x".to_string()), Token::Assign, Token::Number(10), Token::Semicolon,
             Token::Char, Token::Ident("y".to_string()), Token::Assign, Token::CharLiteral('c'), Token::Semicolon,
             Token::Return, Token::Ident("x".to_string()), Token::Semicolon,
             Token::Eof,
         ];
         assert_eq!(tokenize(input), Ok(expected));
     }

    #[test]
    fn test_division_vs_comment() {
        let input = "x = a / b; // divide\n /* block */ y = c*d;";
         let expected = vec![
             Token::Ident("x".to_string()), Token::Assign, Token::Ident("a".to_string()),
             Token::Div, Token::Ident("b".to_string()), Token::Semicolon,
             Token::Ident("y".to_string()), Token::Assign, Token::Ident("c".to_string()),
             Token::Asterisk, Token::Ident("d".to_string()), Token::Semicolon,
             Token::Eof,
         ];
        assert_eq!(tokenize(input), Ok(expected));
    }

    #[test]
    fn test_complex_sequence() {
        let input = "int main() { if (x <= 10) { y = y++ + 1; /* inc */ } return 0; }";
        let expected = vec![
            Token::Int, Token::Ident("main".to_string()), Token::LParen, Token::RParen, Token::LBrace,
            Token::If, Token::LParen, Token::Ident("x".to_string()), Token::Le, Token::Number(10), Token::RParen, Token::LBrace,
            Token::Ident("y".to_string()), Token::Assign, Token::Ident("y".to_string()), Token::Inc, Token::Add, Token::Number(1), Token::Semicolon,
            Token::RBrace, Token::Return, Token::Number(0), Token::Semicolon,
            Token::RBrace, Token::Eof,
        ];
        assert_eq!(tokenize(input), Ok(expected));
    }

    // --- Error Cases ---

     #[test]
     fn test_error_invalid_character() {
         let result = tokenize("int $a = 0;");
         // Position check depends on UTF-8 lengths. '$' likely after 'int '. 4 bytes for 'int ', 1 for space. So position 5.
         assert!(matches!(result, Err(LexerError::InvalidCharacter('$', 4))), "Result was: {:?}", result); // Check char and approx position
     }

     #[test]
     fn test_error_unterminated_string() {
         let result = tokenize("char *s = \"hello");
         // String starts after 'char *s = '. Let's count bytes: 4+1+1+1+1+1+1 = 10. String starts at byte 10.
          assert!(matches!(result, Err(LexerError::UnterminatedString(10))), "Result was: {:?}", result);
     }

     #[test]
     fn test_error_unterminated_char() {
         let result = tokenize("char c = 'a");
         // Char starts after 'char c = '. 4+1+1+1+1+1 = 9. Char starts at byte 9.
         assert!(matches!(result, Err(LexerError::UnterminatedChar(9))), "Result was: {:?}", result);
     }

      #[test]
      fn test_error_empty_char() {
          let result = tokenize("char c = '';"); // Invalid in C
          // Char starts at byte 9.
          assert!(matches!(result, Err(LexerError::UnterminatedChar(9))), "Result was: {:?}", result);
      }

    #[test]
     fn test_error_unterminated_block_comment() {
         let result = tokenize("int x = 1; /* hello");
         // Comment starts after 'int x = 1; '. 4+1+1+1+1+1+1 = 10. Comment starts at byte 11.
         assert!(matches!(result, Err(LexerError::UnterminatedBlockComment(11))), "Result was: {:?}", result);
     }

     #[test]
     fn test_error_invalid_escape_string() {
        let result = tokenize("char *s = \"hello\\x world\";");
        assert!(matches!(result, Err(LexerError::InvalidEscapeSequence('x', 17))), "Result was: {:?}", result);
     }


#[test]
     fn test_error_invalid_escape_char() {
        let result = tokenize("char c = '\\z';");
        assert!(matches!(result, Err(LexerError::InvalidEscapeSequence('z', 11))), "Result was: {:?}", result);
     }
}