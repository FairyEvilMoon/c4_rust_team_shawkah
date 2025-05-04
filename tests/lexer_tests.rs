// src/lexer.rs
// ... (rest of the lexer code) ...

#[cfg(test)]
mod tests {
    // Use super to access items in the parent module (lexer.rs)
    // Or use the full crate path if needed, but super is common for inline tests.
    use c4_rust_team_shawkah::lexer::{Lexer, LexerError, Token, TokenInfo};

    // Helper to create TokenInfo without manual line/col calculation for simple tests
    fn ti(token: Token, line: usize, col: usize) -> TokenInfo {
        TokenInfo { token, line, column: col }
    }

    // Helper function to collect all tokens (excluding EOF)
    fn collect_tokens(source: &str) -> Result<Vec<TokenInfo>, LexerError> {
        let lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        for result in lexer {
            let token_info = result?;
            if token_info.token == Token::Eof {
                break;
            }
            tokens.push(token_info);
        }
        Ok(tokens)
    }

     // Helper function to collect all tokens including EOF
    fn collect_tokens_with_eof(source: &str) -> Result<Vec<TokenInfo>, LexerError> {
        Lexer::new(source).collect()
    }

    #[test]
    fn test_simple_keywords_and_identifiers() {
        let source = "int main return";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Int, 1, 1),
            ti(Token::Ident("main".to_string()), 1, 5),
            ti(Token::Return, 1, 10),
        ]);
        // Check EOF separately if needed, or use collect_tokens_with_eof
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.nth(3).unwrap().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_punctuation() {
        let source = "(){};";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::LParen, 1, 1),
            ti(Token::RParen, 1, 2),
            ti(Token::LBrace, 1, 3),
            ti(Token::RBrace, 1, 4),
            ti(Token::Semicolon, 1, 5),
        ]);
    }

    #[test]
    fn test_integer_literal() {
        let source = "123 0";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Number(123), 1, 1),
            ti(Token::Number(0), 1, 5),
        ]);
    }

     #[test]
    fn test_hex_literal() {
        let source = "0x1A 0Xff 0x0";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Number(0x1A), 1, 1),  // 26
            ti(Token::Number(0xFF), 1, 6),  // 255
            ti(Token::Number(0x0), 1, 11), // 0
        ]);
    }

    #[test]
    fn test_octal_literal() {
        let source = "0123 07 0"; // 0123 (oct) = 83 (dec), 07 (oct) = 7 (dec)
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Number(83), 1, 1),
            ti(Token::Number(7), 1, 6),
            ti(Token::Number(0), 1, 9),
        ]);
    }

     #[test]
    fn test_invalid_octal_literal() {
        let source = "08"; // Invalid octal
        let mut lexer = Lexer::new(source);
        let err = lexer.next_token().unwrap_err();
        assert_eq!(err, LexerError::InvalidNumberFormat("08".to_string(), 1, 1));
    }

    #[test]
    fn test_string_literal() {
        let source = "\"hello world\\n\""; // Raw string contains quotes and escape
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::StringLiteral("hello world\n".to_string()), 1, 1),
        ]);
    }

     #[test]
    fn test_char_literal() {
        let source = "'a' '\\n' '\\''";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::CharLiteral('a'), 1, 1),
            ti(Token::CharLiteral('\n'), 1, 5),
            ti(Token::CharLiteral('\''), 1, 10),
        ]);
    }

    #[test]
    fn test_whitespace_and_newlines() {
        let source = "int main() {\n  return 0;\n}";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Int, 1, 1),
            ti(Token::Ident("main".to_string()), 1, 5),
            ti(Token::LParen, 1, 9),
            ti(Token::RParen, 1, 10),
            ti(Token::LBrace, 1, 12),
            ti(Token::Return, 2, 3),
            ti(Token::Number(0), 2, 10),
            ti(Token::Semicolon, 2, 11),
            ti(Token::RBrace, 3, 1),
        ]);
    }

    #[test]
    fn test_line_comment() {
        let source = "int // ignore this\nmain";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            ti(Token::Int, 1, 1),
            ti(Token::Ident("main".to_string()), 2, 1),
        ]);
    }

     #[test]
    fn test_block_comment() {
        let source = "/* ignore this */ main /* multi\nline */";
        let tokens = collect_tokens(source).unwrap();
        assert_eq!(tokens, vec![
            // The column should be 18, after the "*/ "
            ti(Token::Ident("main".to_string()), 1, 18),
        ]);
    }

     #[test]
    fn test_nested_block_comment_fail() {
        // Standard C doesn't support nested block comments
        let source = "/* outer /* inner */ still outer? */";
        let mut lexer = Lexer::new(source);
        // Should stop after the first "*/", leaving " still outer? */"
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("still".to_string()), 1, 22));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("outer".to_string()), 1, 28));
        // Lexer error on the second '?' or '*' depending on how it tries to parse?
        // Let's expect Asterisk then Div.
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Asterisk, 1, 35)); // Expect '*'
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Div, 1, 36));      // Expect '/'
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);           // Expect EOF
    }


     #[test]
    fn test_invalid_character() {
        let source = "int $";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Int, 1, 1));
        let err = lexer.next_token().unwrap_err();
        assert_eq!(err, LexerError::InvalidCharacter('$', 1, 5));
    }

     #[test]
    fn test_unterminated_string() {
        let source = "\"hello";
        let mut lexer = Lexer::new(source);
        let err = lexer.next_token().unwrap_err();
        // Check the error type and potentially start position
        assert_eq!(err, LexerError::UnterminatedString(1, 1));
     }

     #[test]
    fn test_unterminated_char() {
        let source = "'a";
        let mut lexer = Lexer::new(source);
        let err = lexer.next_token().unwrap_err();
        assert_eq!(err, LexerError::UnterminatedChar(1, 1));
     }

     #[test]
    fn test_empty_char() {
        let source = "''";
        let mut lexer = Lexer::new(source);
        let err = lexer.next_token().unwrap_err();
        assert_eq!(err, LexerError::UnterminatedChar(1, 1)); // Error occurs at first quote
     }

      #[test]
      fn test_operators() {
          let source = "= == + - * / % & && | || ! != < <= > >= << >> **"; // Added **
          // Use collect_tokens_with_eof to check everything including EOF
          let tokens_with_eof = collect_tokens_with_eof(source).unwrap();

          let expected_tokens: Vec<Token> = vec![
              Token::Assign,
              Token::Eq,
              Token::Add,
              Token::Sub,
              Token::Asterisk, // '*' emitted by lexer
              Token::Div,
              Token::Mod,
              Token::BitAnd,   // '&' emitted by lexer as BitAnd *** FIXED ***
              Token::Lan,      // '&&' is Lan          *** FIXED ***
              Token::BitOr,
              Token::Lor,      // '||' is Lor          *** FIXED ***
              Token::Not,
              Token::Ne,
              Token::Lt,
              Token::Le,
              Token::Gt,
              Token::Ge,
              Token::Shl,
              Token::Shr,
              Token::Pow,      // Added '**' check
              Token::Eof       // Included EOF
          ];

          // Extract just the token variants for comparison
          let found_tokens: Vec<Token> = tokens_with_eof.into_iter().map(|ti| ti.token).collect();

          assert_eq!(found_tokens, expected_tokens, "Operator sequence mismatch");
      }

      #[test]
      fn test_preprocessor_directive_skip() {
           let source = "#include <stdio.h>\nint main()";
           let tokens = collect_tokens(source).unwrap();
           assert_eq!(tokens, vec![
                ti(Token::Int, 2, 1), // int starts on line 2, col 1
                ti(Token::Ident("main".to_string()), 2, 5),
                ti(Token::LParen, 2, 9),
                ti(Token::RParen, 2, 10),
           ]);
      }

       #[test]
      fn test_empty_input() {
            let source = "";
            let mut lexer = Lexer::new(source);
            assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
            // Check that subsequent calls also return EOF (or None if using Iterator strictly)
            assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
      }

       #[test]
       fn test_only_whitespace_and_comments() {
            let source = "  \n// comment\n/* block \n comment */\n  ";
            let mut lexer = Lexer::new(source);
            assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
       }
}