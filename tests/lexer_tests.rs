// src/lexer.rs
// ... (rest of the lexer code) ...

#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::lexer::{Lexer, LexerError, Token, TokenInfo};// Import items from the outer module (Lexer, Token, etc.)

    // Helper to create TokenInfo without manual line/col calculation for simple tests
    fn ti(token: Token, line: usize, col: usize) -> TokenInfo {
        TokenInfo { token, line, column: col }
    }

    #[test]
    fn test_simple_keywords_and_identifiers() {
        let source = "int main return";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Int, 1, 1));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("main".to_string()), 1, 5));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Return, 1, 10));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof); // Check EOF
    }

    #[test]
    fn test_punctuation() {
        let source = "(){};";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::LParen, 1, 1));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::RParen, 1, 2));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::LBrace, 1, 3));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::RBrace, 1, 4));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Semicolon, 1, 5));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_integer_literal() {
        let source = "123 0";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Number(123), 1, 1));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Number(0), 1, 5));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_string_literal() {
        let source = "\"hello world\\n\""; // Raw string contains quotes and escape
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::StringLiteral("hello world\n".to_string()), 1, 1));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_whitespace_and_newlines() {
        let source = "int main() {\n  return 0;\n}";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Int, 1, 1));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("main".to_string()), 1, 5));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::LParen, 1, 9));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::RParen, 1, 10));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::LBrace, 1, 12));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Return, 2, 3)); // Note line/col
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Number(0), 2, 10));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Semicolon, 2, 11));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::RBrace, 3, 1));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
    }

    #[test]
    fn test_line_comment() {
        let source = "int // ignore this\nmain";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Int, 1, 1));
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("main".to_string()), 2, 1));
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
    }

     #[test]
    fn test_block_comment() {
        let source = "/* ignore this */ main /* multi\nline */";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.next_token().unwrap(), ti(Token::Ident("main".to_string()), 1, 18)); // After first comment
        // No more tokens after the second comment
        assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
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
        assert!(matches!(err, LexerError::UnterminatedString(1, 1)));
     }

      #[test]
      fn test_operators() {
          // Test a few operators the lexer supports, even if parser doesn't use them yet
          let source = "= == + - * / % & && | || ! != < <= > >= << >>";
          let mut lexer = Lexer::new(source);
          assert_eq!(lexer.next_token().unwrap().token, Token::Assign);
          assert_eq!(lexer.next_token().unwrap().token, Token::Eq);
          assert_eq!(lexer.next_token().unwrap().token, Token::Add);
          assert_eq!(lexer.next_token().unwrap().token, Token::Sub);
          assert_eq!(lexer.next_token().unwrap().token, Token::Asterisk); // '*' is ambiguous
          assert_eq!(lexer.next_token().unwrap().token, Token::Div);
          assert_eq!(lexer.next_token().unwrap().token, Token::Mod);
          assert_eq!(lexer.next_token().unwrap().token, Token::Ampersand); // '&' is ambiguous
          assert_eq!(lexer.next_token().unwrap().token, Token::LogAnd);
          assert_eq!(lexer.next_token().unwrap().token, Token::BitOr);
          assert_eq!(lexer.next_token().unwrap().token, Token::LogOr);
          assert_eq!(lexer.next_token().unwrap().token, Token::Not);
          assert_eq!(lexer.next_token().unwrap().token, Token::Ne);
          assert_eq!(lexer.next_token().unwrap().token, Token::Lt);
          assert_eq!(lexer.next_token().unwrap().token, Token::Le);
          assert_eq!(lexer.next_token().unwrap().token, Token::Gt);
          assert_eq!(lexer.next_token().unwrap().token, Token::Ge);
          assert_eq!(lexer.next_token().unwrap().token, Token::Shl);
          assert_eq!(lexer.next_token().unwrap().token, Token::Shr);
          assert_eq!(lexer.next_token().unwrap().token, Token::Eof);
      }
}