//! Lexer (Tokenizer) for the C4 subset of C.

use std::iter::Peekable;
use std::str::Chars;

//Token Definition (Refined)
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,

    // Keywords
    Enum, If, Else, Int, Char, Return, Sizeof, While,

    // Identifiers
    Ident(String),

    // Literals
    Number(i64),
    StringLiteral(String),
    CharLiteral(char),

    // Operators & Punctuation
    Assign,     // =
    Eq, Ne,     // ==, !=
    Lt, Gt, Le, Ge, // <, >, <=, >=
    Add, Sub, Mul, Div, Mod, // +, -, *, /, %
    Inc, Dec,   // ++, --
    BitOr, BitXor, BitAnd, // |, ^, &
    LogOr, LogAnd, Not,    // ||, &&, !
    Shl, Shr,   // <<, >>

    // Single char symbols / Ambiguous
    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket, // ;, ,, (, ), {, }, [, ]
    Asterisk, // *, ambiguous (Mul or Deref)
    Ampersand, // &, ambiguous (BitAnd or AddressOf)
}

//Lexer Error (Refined with Positions)
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    InvalidCharacter(char, usize),     // Character and starting byte position
    UnterminatedString(usize),         // Starting byte position of the string
    UnterminatedChar(usize),           // Starting byte position of the char literal
    UnterminatedBlockComment(usize),   // Starting byte position of the block comment
    InvalidEscapeSequence(char, usize),// The invalid character after '\' and its byte position
}

//Lexer Implementation (Refined)
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_pos: usize, // Tracks the current byte position for error reporting
    source: &'a str,    // Keep reference to original source if needed
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer.
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            current_pos: 0,
            source: input,
        }
    }

    /// Consumes the next character and updates the byte position.
    fn consume(&mut self) -> Option<char> {
        let next_char = self.input.next();
        if let Some(c) = next_char {
            self.current_pos += c.len_utf8(); // Track byte position
        }
        next_char
    }

    /// Peeks at the next character without consuming it.
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    /// Consumes the next character *if* it matches the expected one.
    fn consume_if_eq(&mut self, expected: char) -> bool {
        if self.peek() == Some(&expected) {
            self.consume();
            true
        } else {
            false
        }
    }

    /// Reads a sequence of digits into a number token.
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
            Err(_) => panic!(
                "Internal Lexer Error: Failed to parse valid number string '{}'",
                number_str
            ),
        }
    }

    /// Reads an identifier or determines if it's a keyword.
    fn read_identifier_or_keyword(&mut self, first_char: char) -> Result<Token, LexerError> {
        let mut ident = String::new();
        ident.push(first_char);

        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        // Check if it's a keyword
        match ident.as_str() {
            "enum" => Ok(Token::Enum),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            "int" => Ok(Token::Int),
            "char" => Ok(Token::Char),
            "return" => Ok(Token::Return),
            "sizeof" => Ok(Token::Sizeof),
            "while" => Ok(Token::While),
            // Add other C4 keywords here if any
            _ => Ok(Token::Ident(ident)), // Otherwise, it's an identifier
        }
    }

    /// Reads a string literal delimited by double quotes. Handles basic escapes.
    fn read_string_literal(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.current_pos - 1; // Position of the opening quote
        let mut literal = String::new();
        loop {
            match self.consume() {
                Some('"') => return Ok(Token::StringLiteral(literal)),
                Some('\\') => {
                    let escape_pos = self.current_pos; // Position BEFORE consuming escaped char
                    match self.consume() {
                        Some('n') => literal.push('\n'),
                        Some('t') => literal.push('\t'),
                        Some('"') => literal.push('"'),
                        Some('\\') => literal.push('\\'),
                        Some('0') => literal.push('\0'),
                        Some(other) => return Err(LexerError::InvalidEscapeSequence(other, escape_pos)),
                        None => return Err(LexerError::UnterminatedString(start_pos)),
                    }
                }
                Some(c) => literal.push(c),
                None => return Err(LexerError::UnterminatedString(start_pos)), // Reached EOF
            }
        }
    }

    fn read_char_literal(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.current_pos - 1; // Position of the opening quote
        let char_val = match self.consume() {
            Some('\\') => {
                let escape_pos = self.current_pos; // Position BEFORE consuming escaped char
                match self.consume() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('\'') => '\'',
                    Some('\\') => '\\',
                    Some('0') => '\0',
                     // Add other C escapes if C4 supports them
                    Some(other) => return Err(LexerError::InvalidEscapeSequence(other, escape_pos)),
                    None => return Err(LexerError::UnterminatedChar(start_pos)),
                }
            }
            Some('\'') => return Err(LexerError::UnterminatedChar(start_pos)), // Empty char literal '' is invalid
            Some(c) => c,
            None => return Err(LexerError::UnterminatedChar(start_pos)), // Reached EOF
        };

        if !self.consume_if_eq('\'') {
            return Err(LexerError::UnterminatedChar(start_pos));
        }

        Ok(Token::CharLiteral(char_val))
    }

    /// Gets the next token from the input stream, skipping whitespace and comments.
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        loop {
            // Skip whitespace inline
            while let Some(&c) = self.peek() {
                if c.is_whitespace() {
                    self.consume();
                } else {
                    break;
                }
            }

            // Handle comments inline
             let comment_start_pos = self.current_pos;
             if self.peek() == Some(&'/') {
                 self.consume(); // Consume first '/'
                 if self.consume_if_eq('/') { // Line comment
                     while let Some(c)=self.consume(){ if c=='\n'{break;}}
                     continue; // Restart main loop
                 }
                 else if self.consume_if_eq('*') { // Block comment
                     let mut maybe_end = false;
                     loop{
                         match self.consume(){
                             Some('*')=> maybe_end=true,
                             Some('/') if maybe_end => break, // Found */
                             Some(_)=> maybe_end=false,
                             None => return Err(LexerError::UnterminatedBlockComment(comment_start_pos))
                         }
                     }
                     continue; // Restart main loop
                 }
                 else {
                     // It was just division, not a comment
                     return Ok(Token::Div);
                 }
             }

             //Regular token processing
             let token_start_pos = self.current_pos;
             match self.consume() {
                 // Handle multi-character operators first
                 Some('=') => return Ok(if self.consume_if_eq('=') { Token::Eq } else { Token::Assign }),
                 Some('!') => return Ok(if self.consume_if_eq('=') { Token::Ne } else { Token::Not }),
                 Some('<') => return Ok(if self.consume_if_eq('=') { Token::Le } else if self.consume_if_eq('<') { Token::Shl } else { Token::Lt }),
                 Some('>') => return Ok(if self.consume_if_eq('=') { Token::Ge } else if self.consume_if_eq('>') { Token::Shr } else { Token::Gt }),
                 Some('+') => return Ok(if self.consume_if_eq('+') { Token::Inc } else { Token::Add }),
                 Some('-') => return Ok(if self.consume_if_eq('-') { Token::Dec } else { Token::Sub }),
                 Some('&') => return Ok(if self.consume_if_eq('&') { Token::LogAnd } else { Token::Ampersand }), // Note: Ampersand is ambiguous
                 Some('|') => return Ok(if self.consume_if_eq('|') { Token::LogOr } else { Token::BitOr }),

                 // Single-character operators / Punctuation that were not part of multi-char
                 Some('*') => return Ok(Token::Asterisk), // Note: Asterisk is ambiguous
                 Some('%') => return Ok(Token::Mod),
                 Some('^')=> return Ok(Token::BitXor),
                 // '/' was handled above with comments
                 Some(';') => return Ok(Token::Semicolon), Some(',') => return Ok(Token::Comma),
                 Some('(') => return Ok(Token::LParen), Some(')') => return Ok(Token::RParen),
                 Some('{') => return Ok(Token::LBrace), Some('}') => return Ok(Token::RBrace),
                 Some('[') => return Ok(Token::LBracket), Some(']') => return Ok(Token::RBracket),

                 // Literals / Identifiers
                 Some('"') => return self.read_string_literal(),
                 Some('\'') => return self.read_char_literal(),
                 Some(c @ '0'..='9') => return self.read_number(c),
                 Some(c @ ('a'..='z' | 'A'..='Z' | '_')) => return self.read_identifier_or_keyword(c),

                 // End Of File
                 None => return Ok(Token::Eof),

                 // Error case
                 Some(c) => return Err(LexerError::InvalidCharacter(c, token_start_pos)), // Include position
            }
        } // End loop
    }

    /// Optional: Helper to tokenize the entire input at once.
    #[allow(dead_code)]
    pub fn tokenize_all(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token == Token::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }
}