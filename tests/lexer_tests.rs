#[cfg(test)]
mod tests {
    use c4_rust_team_shawkah::lexer::{Lexer, Token, LexerError};

    // Helper function (same as Commit 1)
    fn tokenize(input: &str) -> Vec<Token> { /* ... */ let mut l=Lexer::new(input); let mut t=Vec::new(); loop {match l.next_token(){ Ok(Token::Eof)=>{t.push(Token::Eof); break;} Ok(tok)=>{t.push(tok);} Err(_)=>{break;}}} t}


    #[test] fn test_empty_input() { /* ... */ assert_eq!(tokenize(""), vec![Token::Eof]); }
    #[test] fn test_only_whitespace() { /* ... */ assert_eq!(tokenize("  \n\t  "), vec![Token::Eof]); }
    #[test] fn test_keywords() { /* ... same as Commit 1 ... */ let i="enum if else int char return sizeof while"; let e=vec![Token::Enum, Token::If, Token::Else, Token::Int, Token::Char,Token::Return, Token::Sizeof, Token::While, Token::Eof]; assert_eq!(tokenize(i), e); }
    #[test] fn test_single_char_symbols() { /* ... same as Commit 1 ... */ let i=";(){}=+-"; let e=vec![Token::Semicolon, Token::LParen, Token::RParen, Token::LBrace, Token::RBrace, Token::Assign, Token::Add, Token::Sub, Token::Eof]; assert_eq!(tokenize(i),e);}


    // <-- Added test for numbers -->
    #[test]
    fn test_numbers() {
        let input = "123 0 9876543210 42";
        let expected = vec![
            Token::Number(123), Token::Number(0), Token::Number(9876543210), Token::Number(42), Token::Eof,
        ];
        assert_eq!(tokenize(input), expected);
    }

    #[test] fn test_numbers_and_keywords() {
        let input = "int 10 return 0";
         let expected = vec![Token::Int, Token::Number(10), Token::Return, Token::Number(0), Token::Eof];
        assert_eq!(tokenize(input), expected);
    }

     #[test] fn test_error_invalid_character() { /* ... */ let mut l=Lexer::new("$"); assert!(matches!(l.next_token(), Err(LexerError::InvalidCharacter('$'))));}

}