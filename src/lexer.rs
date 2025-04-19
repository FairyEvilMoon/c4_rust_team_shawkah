//! Lexer (Tokenizer) for the C4 subset of C
use std::iter::Peekable;
use std::str::Chars;

// --- Token Definition ---
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    Enum, If, Else, Int, Char, Return, Sizeof, While,
    Ident(String),
    Number(i64),
    StringLiteral(String), // <-- Added
    CharLiteral(char),     // <-- Added
    Assign, Eq, Lt, Gt, Add, Sub, Mul, Div, Mod,
    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket,
}

// --- Lexer Error ---
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    InvalidCharacter(char),
    UnterminatedString, // <-- Added
    UnterminatedChar,   // <-- Added
    InvalidEscapeSequence, // <-- Added
}

// --- Lexer Implementation ---
pub struct Lexer<'a> { /* ... same as Commit 3 ... */ input: Peekable<Chars<'a>> }

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self { /* ... */ Lexer { input: input.chars().peekable() } }
    fn consume(&mut self) -> Option<char> { /* ... */ self.input.next() }
    fn peek(&mut self) -> Option<&char> { /* ... */ self.input.peek() }
    fn skip_whitespace(&mut self) { /* ... */ while let Some(&c)=self.peek() {if c.is_whitespace(){self.consume();}else{break;}}}
    fn read_number(&mut self, first_digit: char) -> Result<Token, LexerError> { /* ... same as Commit 3 ... */ let mut n=String::new(); n.push(first_digit); while let Some(&c)=self.peek(){if c.is_digit(10){n.push(self.consume().unwrap());}else{break;}} Ok(Token::Number(n.parse().unwrap()))}
    fn read_identifier_or_keyword(&mut self, first_char: char) -> Result<Token, LexerError> { /* ... same as Commit 3 ... */ let mut i=String::new(); i.push(first_char); while let Some(&c)=self.peek(){if c.is_alphanumeric()||c=='_'{i.push(self.consume().unwrap());}else{break;}}Ok(match i.as_str(){ "enum"=>Token::Enum,"if"=>Token::If,"else"=>Token::Else,"int"=>Token::Int,"char"=>Token::Char,"return"=>Token::Return,"sizeof"=>Token::Sizeof,"while"=>Token::While, _=>Token::Ident(i)})}


    // <-- Added String literal helper -->
    fn read_string_literal(&mut self) -> Result<Token, LexerError> {
        let mut literal = String::new();
        // Assumes opening " is consumed
        loop {
            match self.consume() {
                Some('"') => return Ok(Token::StringLiteral(literal)),
                Some('\\') => {
                    match self.consume() { // Handle escape sequences
                        Some('n') => literal.push('\n'),
                        Some('t') => literal.push('\t'),
                        Some('"') => literal.push('"'),
                        Some('\\') => literal.push('\\'),
                        Some('0') => literal.push('\0'),
                        Some(_) => return Err(LexerError::InvalidEscapeSequence), // Basic error
                        None => return Err(LexerError::UnterminatedString),
                    }
                }
                Some(c) => literal.push(c),
                None => return Err(LexerError::UnterminatedString), // Reached EOF
            }
        }
    }

     // <-- Added Char literal helper -->
    fn read_char_literal(&mut self) -> Result<Token, LexerError> {
        // Assumes opening ' is consumed
        let char_val = match self.consume() {
            Some('\\') => {
                match self.consume() { // Handle escape sequences
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('\'') => '\'',
                    Some('\\') => '\\',
                    Some('0') => '\0',
                    Some(_) => return Err(LexerError::InvalidEscapeSequence),
                    None => return Err(LexerError::UnterminatedChar),
                }
            }
            Some('\'') => return Err(LexerError::UnterminatedChar), // Empty char ''
            Some(c) => c,
            None => return Err(LexerError::UnterminatedChar), // Reached EOF
        };

        // Expect closing quote
        match self.consume() {
             Some('\'') => Ok(Token::CharLiteral(char_val)),
             _ => Err(LexerError::UnterminatedChar), // Missing or wrong closer
        }
    }


    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        if let Some(&next_char) = self.peek() {
            match next_char {
                 // <-- Added cases for literals -->
                 '"' => { self.consume(); self.read_string_literal() }
                 '\'' => { self.consume(); self.read_char_literal() }

                 '=' | '<' | '>' | '+' | '-' | '*' | '/' | '%' | ';' | ',' | '(' | ')' | '{' | '}' | '[' | ']' => {
                     let c = self.consume().unwrap();
                     match c { '=' => Ok(Token::Assign),/*..others..*/';'=>Ok(Token::Semicolon), _=>unreachable!() }
                }
                '0'..='9' => {
                    let first = self.consume().unwrap(); self.read_number(first)
                }
                 'a'..='z' | 'A'..='Z' | '_' => {
                    let first = self.consume().unwrap(); self.read_identifier_or_keyword(first)
                }
                _ => { let c = self.consume().unwrap(); Err(LexerError::InvalidCharacter(c)) }
            }
        } else { Ok(Token::Eof) }
    }
}