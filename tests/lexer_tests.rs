#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::lexer::{Lexer, Token, LexerError};

    fn tokenize(input: &str) -> Result<Vec<Token>, LexerError> { // Changed to return Result
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token()?; // Use ? for error propagation
            let is_eof = token == Token::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    #[test] fn test_empty_input() { assert_eq!(tokenize(""), Ok(vec![Token::Eof])); }
    #[test] fn test_only_whitespace() { assert_eq!(tokenize(" "), Ok(vec![Token::Eof])); }
    #[test] fn test_keywords() { let i="enum while"; let e=vec![Token::Enum,Token::While,Token::Eof]; assert_eq!(tokenize(i), Ok(e));}
    #[test] fn test_single_char_symbols() { let i=";(){}"; let e=vec![Token::Semicolon,Token::LParen,Token::RParen,Token::LBrace,Token::RBrace,Token::Eof]; assert_eq!(tokenize(i), Ok(e)); }
    #[test] fn test_numbers() { let i="123 0"; let e=vec![Token::Number(123),Token::Number(0),Token::Eof]; assert_eq!(tokenize(i), Ok(e));}
    #[test] fn test_identifiers() { let i="v _a"; let e=vec![Token::Ident("v".to_string()),Token::Ident("_a".to_string()),Token::Eof]; assert_eq!(tokenize(i), Ok(e));}


    // <-- Added literal tests -->
    #[test]
    fn test_string_literals() {
        let input = r#" "hello" "" "with\nescape" "and \"quotes\"" "#;
        let expected = vec![
            Token::StringLiteral("hello".to_string()),
            Token::StringLiteral("".to_string()),
            Token::StringLiteral("with\nescape".to_string()),
            Token::StringLiteral("and \"quotes\"".to_string()),
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


    // <-- Added literal error tests -->
    #[test]
     fn test_error_unterminated_string() {
         let result = tokenize("char *s = \"hello");
          assert!(matches!(result, Err(LexerError::UnterminatedString)));
     }

     #[test]
     fn test_error_unterminated_char() {
         let result = tokenize("char c = 'a");
         assert!(matches!(result, Err(LexerError::UnterminatedChar)));
     }
      #[test]
      fn test_error_empty_char() {
          let result = tokenize("char c = '';");
          assert!(matches!(result, Err(LexerError::UnterminatedChar)));
      }
     #[test]
     fn test_error_invalid_escape_string() {
        let result = tokenize("\"hello\\x world\"");
        assert!(matches!(result, Err(LexerError::InvalidEscapeSequence)));
     }
      #[test]
      fn test_error_invalid_escape_char() {
         let result = tokenize("'\\z'");
         assert!(matches!(result, Err(LexerError::InvalidEscapeSequence)));
     }

    #[test] fn test_error_invalid_character() { let r=tokenize("$"); assert!(matches!(r, Err(LexerError::InvalidCharacter('$')))); }


}