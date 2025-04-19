//! Lexer (Tokenizer) for the C4 subset of C - Stage 1: Basics

use std::iter::Peekable;
use std::str::Chars;

// --- Token Definition (Basic) ---
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,

    // Keywords
    Enum, If, Else, Int, Char, Return, Sizeof, While,

    // Basic Symbols & Single-char Operators (Operators refined later)
    Assign,     // =
    Eq,         // == (Placeholder, just handles single '=' for now)
    Lt, Gt,     // <, >
    Add, Sub, Mul, Div, Mod, // +, -, *, /, %

    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket,
}

// --- Lexer Error (Basic) ---
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    InvalidCharacter(char),
}

// --- Lexer Implementation (Basic) ---
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    // current_pos: usize, // Position tracking added later
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            // current_pos: 0,
        }
    }

    fn consume(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c.is_whitespace() {
                self.consume();
            } else {
                break;
            }
        }
    }

    // Temporary simple identifier/keyword reader
    fn read_potential_keyword(&mut self, first_char: char) -> String {
        let mut ident = String::new();
        ident.push(first_char);
        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' { // Basic identifier chars
                ident.push(self.consume().unwrap());
            } else {
                break;
            }
        }
        ident
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        match self.consume() {
            Some('=') => Ok(Token::Assign), // Simple assign for now
            Some('<') => Ok(Token::Lt),
            Some('>') => Ok(Token::Gt),
            Some('+') => Ok(Token::Add),
            Some('-') => Ok(Token::Sub),
            Some('*') => Ok(Token::Mul),
            Some('/') => Ok(Token::Div),
            Some('%') => Ok(Token::Mod),
            Some(';') => Ok(Token::Semicolon),
            Some(',') => Ok(Token::Comma),
            Some('(') => Ok(Token::LParen),
            Some(')') => Ok(Token::RParen),
            Some('{') => Ok(Token::LBrace),
            Some('}') => Ok(Token::RBrace),
            Some('[') => Ok(Token::LBracket),
            Some(']') => Ok(Token::RBracket),

            Some(c @ ('a'..='z' | 'A'..='Z' | '_')) => {
                let ident = self.read_potential_keyword(c);
                // Check keywords
                match ident.as_str() {
                    "enum" => Ok(Token::Enum),
                    "if" => Ok(Token::If),
                    "else" => Ok(Token::Else),
                    "int" => Ok(Token::Int),
                    "char" => Ok(Token::Char),
                    "return" => Ok(Token::Return),
                    "sizeof" => Ok(Token::Sizeof),
                    "while" => Ok(Token::While),
                    _ => Err(LexerError::InvalidCharacter(c)) // Treat unknown identifiers as error for now
                }
            }

            None => Ok(Token::Eof),
            Some(c) => Err(LexerError::InvalidCharacter(c)),
        }
    }
}