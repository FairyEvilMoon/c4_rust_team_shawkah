//! Lexer (Tokenizer) for the C4 subset of C - Stage 2: Numbers

use std::iter::Peekable;
use std::str::Chars;

// --- Token Definition ---
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    Enum, If, Else, Int, Char, Return, Sizeof, While,
    Number(i64), // <-- Added
    Assign, Eq, Lt, Gt, Add, Sub, Mul, Div, Mod,
    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket,
}

// --- Lexer Error ---
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    InvalidCharacter(char),
}

// --- Lexer Implementation ---
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self { /* ... same as Commit 1 ... */ Lexer { input: input.chars().peekable() } }
    fn consume(&mut self) -> Option<char> { /* ... same as Commit 1 ... */ self.input.next() }
    fn peek(&mut self) -> Option<&char> { /* ... same as Commit 1 ... */ self.input.peek() }
    fn skip_whitespace(&mut self) { /* ... same as Commit 1 ... */ while let Some(&c) = self.peek() { if c.is_whitespace() { self.consume(); } else { break; }} }
    fn read_potential_keyword(&mut self, first_char: char) -> String { /* ... same as Commit 1 ... */ let mut i = String::new(); i.push(first_char); while let Some(&c)=self.peek() { if c.is_alphanumeric()||c=='_' { i.push(self.consume().unwrap());} else {break;}} i}


    // <-- Added helper for numbers -->
    fn read_number(&mut self, first_digit: char) -> Result<Token, LexerError> {
        let mut number_str = String::new();
        number_str.push(first_digit);

        while let Some(&c) = self.peek() {
            if c.is_digit(10) {
                number_str.push(self.consume().unwrap());
            } else {
                break;
            }
        }
        match number_str.parse::<i64>() {
            Ok(num) => Ok(Token::Number(num)),
            Err(_) => panic!("Internal Lexer Error parsing number"), // Should not happen
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        // Need to peek before consuming for numbers/keywords
        if let Some(&next_char) = self.peek() {
            match next_char {
                '=' | '<' | '>' | '+' | '-' | '*' | '/' | '%' | ';' | ',' | '(' | ')' | '{' | '}' | '[' | ']' => {
                    // Handle single-char ops/symbols by consuming
                    let c = self.consume().unwrap();
                    match c {
                         '=' => Ok(Token::Assign), '<' => Ok(Token::Lt), '>' => Ok(Token::Gt),
                         '+' => Ok(Token::Add), '-' => Ok(Token::Sub), '*' => Ok(Token::Mul),
                         '/' => Ok(Token::Div), '%' => Ok(Token::Mod), ';' => Ok(Token::Semicolon),
                         ',' => Ok(Token::Comma), '(' => Ok(Token::LParen), ')' => Ok(Token::RParen),
                         '{' => Ok(Token::LBrace), '}' => Ok(Token::RBrace), '[' => Ok(Token::LBracket),
                         ']' => Ok(Token::RBracket),
                         _ => unreachable!(), // Should be covered by peek
                    }
                }
                '0'..='9' => {
                    // Handle numbers
                    let first = self.consume().unwrap(); // Consume the digit
                    self.read_number(first) // Delegate
                }
                 'a'..='z' | 'A'..='Z' | '_' => {
                    // Handle keywords (identifiers later)
                     let first = self.consume().unwrap();
                    let ident = self.read_potential_keyword(first);
                     match ident.as_str() {
                        "enum" => Ok(Token::Enum), "if" => Ok(Token::If), "else" => Ok(Token::Else),
                        "int" => Ok(Token::Int), "char" => Ok(Token::Char), "return" => Ok(Token::Return),
                        "sizeof" => Ok(Token::Sizeof), "while" => Ok(Token::While),
                        _ => Err(LexerError::InvalidCharacter(first)) // Error on non-keyword ident for now
                    }
                }
                _ => {
                    // Unknown character
                    let c = self.consume().unwrap();
                    Err(LexerError::InvalidCharacter(c))
                }
            }
        } else {
            Ok(Token::Eof) // End of file
        }
    }
}