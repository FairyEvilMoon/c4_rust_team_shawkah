#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::lexer::{Lexer, Token, LexerError};

    // Helper to collect all tokens (may consume errors early)
    fn tokenize(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            match lexer.next_token() {
                Ok(Token::Eof) => {
                    tokens.push(Token::Eof);
                    break;
                }
                Ok(token) => tokens.push(token),
                Err(_) => break, // Stop on first error for simplicity now
            }
        }
        tokens
    }

    #[test]
    fn test_empty_input() {
        assert_eq!(tokenize(""), vec![Token::Eof]);
    }

    #[test]
    fn test_only_whitespace() {
        assert_eq!(tokenize("  \n\t  "), vec![Token::Eof]);
    }

    #[test]
    fn test_keywords() {
        let input = "enum if else int char return sizeof while";
        let expected = vec![
            Token::Enum, Token::If, Token::Else, Token::Int, Token::Char,
            Token::Return, Token::Sizeof, Token::While, Token::Eof,
        ];
        assert_eq!(tokenize(input), expected);
    }

     #[test]
    fn test_single_char_symbols() {
        let input = "; , ( ) { } [ ] = < > + - * / %";
        let expected = vec![
            Token::Semicolon, Token::Comma, Token::LParen, Token::RParen, Token::LBrace,
            Token::RBrace, Token::LBracket, Token::RBracket,
            Token::Assign, Token::Lt, Token::Gt, Token::Add, Token::Sub, Token::Mul, Token::Div, Token::Mod,
            Token::Eof,
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test]
    fn test_error_invalid_character() {
         let mut lexer = Lexer::new("$"); // Using lexer directly to check error
         assert!(matches!(lexer.next_token(), Err(LexerError::InvalidCharacter('$'))));
    }
}