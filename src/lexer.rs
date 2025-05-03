//! Lexer (Tokenizer) for the C4 subset of C, extended for self-hosting requirements.

use std::iter::Peekable;
use std::str::Chars;

// --- Token Definition (Covers C4 subset + additions) ---
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,

    // Keywords (Ensure all C4 keywords are here)
    Enum, If, Else, Int, Char, Return, Sizeof, While,
    Void, // Added Void if needed as a type keyword

    // Identifiers
    Ident(String),

    // Literals
    Number(i64), // Using i64 for flexibility, original C4 might use i32
    StringLiteral(String),
    CharLiteral(char), // Single character literal

    // Operators & Punctuation (Grouped roughly by type/precedence)
    Assign,     // =
    Cond,       // ?
    Colon,      // :
    Lor,        // ||
    Lan,        // &&
    BitOr,      // | (Bitwise OR)
    BitXor,     // ^
    BitAnd,     // & (Binary Bitwise AND - distinguished from Ampersand for AddressOf)
    Eq, Ne,     // ==, !=
    Lt, Gt, Le, Ge, // <, >, <=, >=
    Shl, Shr,   // <<, >>
    Add, Sub,   // +, - (Binary)
    Mul, Div, Mod, // *, /, % (Binary)
    Inc, Dec,   // ++, --
    Not,        // ! (Logical NOT)
    BitNot,     // ~ (Bitwise NOT)

    // Single char symbols / Ambiguous until parsed
    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket, // ;, ,, (, ), {, }, [, ]
    /// Represents the '*' character. Ambiguous: Multiplication, Dereference, or Pointer type declaration.
    Asterisk,
    /// Represents the '&' character. Ambiguous: Bitwise AND (handled by BitAnd), or AddressOf.
    Ampersand, // Use this for unary AddressOf context

    // Special token for system calls if needed (Parser uses SymbolClass::Sys)
    Sys, // May not be strictly necessary if parser handles via symbol table
}


// --- Token Information (Includes position) ---
#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    pub token: Token,
    pub line: usize,
    pub column: usize,
}

// --- Lexer Error (Includes positions) ---
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    InvalidCharacter(char, usize, usize),
    UnterminatedString(usize, usize),
    UnterminatedChar(usize, usize),
    UnterminatedBlockComment(usize, usize),
    InvalidEscapeSequence(char, usize, usize),
    InvalidNumberFormat(String, usize, usize), // For issues during number parsing
    // Consider adding IntegerOverflow if i64 parsing fails
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::InvalidCharacter(c, l, col) => write!(f, "Invalid character '{}' at {}:{}", c, l, col),
            LexerError::UnterminatedString(l, col) => write!(f, "Unterminated string literal starting at {}:{}", l, col),
            LexerError::UnterminatedChar(l, col) => write!(f, "Unterminated character literal starting at {}:{}", l, col),
            LexerError::UnterminatedBlockComment(l, col) => write!(f, "Unterminated block comment starting at {}:{}", l, col),
            LexerError::InvalidEscapeSequence(c, l, col) => write!(f, "Invalid escape sequence '\\{}' at {}:{}", c, l, col),
            LexerError::InvalidNumberFormat(s, l, col) => write!(f, "Invalid number format '{}' at {}:{}", s, l, col),
        }
    }
}
impl std::error::Error for LexerError {}

// --- Lexer Implementation ---
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    current_line: usize,
    current_col: usize,
    // Removed byte position trackers for simplicity, rely on line/col
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            current_line: 1,
            current_col: 1,
        }
    }

    #[inline]
    fn consume(&mut self) -> Option<char> {
        match self.input.next() {
            Some(c) => {
                if c == '\n' {
                    self.current_line += 1;
                    self.current_col = 1;
                } else {
                    // Handle tabs potentially if needed, otherwise simple increment
                    self.current_col += 1;
                }
                Some(c)
            }
            None => None,
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    // Removed consume_if_eq as it was causing borrow issues in next_token

    #[inline]
     fn token_info(&self, token: Token, start_line: usize, start_col: usize) -> TokenInfo {
        TokenInfo { token, line: start_line, column: start_col }
     }

    fn read_number(&mut self, first_char: char, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut number_str = String::new();
        number_str.push(first_char);
        let base = 10; // Assume decimal unless prefix indicates otherwise

        // C4 doesn't explicitly support 0x or 0 prefixes for hex/octal in its grammar description,
        // but the code handles it. Let's keep the simpler C4 lexer logic first.
        // Revert to simpler C4 logic (no hex/octal detection here):
        while let Some(&c) = self.peek() {
            if c.is_ascii_digit() {
                 number_str.push(self.consume().unwrap());
            } else {
                break;
            }
        }
        // C4 code itself has hex/octal parsing logic, which is complex.
        // Let's stick to decimal for now unless C4 code parsing fails.
        match number_str.parse::<i64>() {
             Ok(num) => Ok(self.token_info(Token::Number(num), start_line, start_col)),
             Err(_) => Err(LexerError::InvalidNumberFormat(number_str, start_line, start_col)),
        }
        // TODO: Add hex/octal parsing logic from C4 if needed later, mirroring its complexity.
    }


    fn read_identifier_or_keyword(&mut self, first_char: char, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut ident = String::new();
        ident.push(first_char);

        while let Some(&c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        // Check if it's a keyword
        let token = match ident.as_str() {
            "char" => Token::Char,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "if" => Token::If,
            "int" => Token::Int,
            "return" => Token::Return,
            "sizeof" => Token::Sizeof,
            "while" => Token::While,
            "void" => Token::Void, // Added void
            _ => Token::Ident(ident), // Not a keyword
        };
        Ok(self.token_info(token, start_line, start_col))
    }

    fn read_string_literal(&mut self, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut literal = String::new();
        loop {
            // FIX: Prefix unused variables with underscore
            let _escape_char_line = self.current_line; // Position before potential consume
            let _escape_char_col = self.current_col;

            match self.consume() {
                Some('"') => return Ok(self.token_info(Token::StringLiteral(literal), start_line, start_col)),
                Some('\\') => {
                     // Use position *after* backslash for escape sequence errors
                    let esc_seq_line = self.current_line;
                    let esc_seq_col = self.current_col;
                    match self.consume() { // Consume character after backslash
                        Some('n') => literal.push('\n'),
                        Some('t') => literal.push('\t'),
                        Some('\\') => literal.push('\\'),
                        Some('"') => literal.push('"'),
                        Some('0') => literal.push('\0'),
                        // C4 doesn't handle other escapes like \' in strings
                        Some(other) => return Err(LexerError::InvalidEscapeSequence(other, esc_seq_line, esc_seq_col)),
                        None => return Err(LexerError::UnterminatedString(start_line, start_col)),
                    }
                }
                Some('\n') => return Err(LexerError::UnterminatedString(start_line, start_col)), // Newline in string literal is error
                Some(c) => literal.push(c),
                None => return Err(LexerError::UnterminatedString(start_line, start_col)),
            }
        }
    }

    fn read_char_literal(&mut self, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let char_val = match self.consume() {
            Some('\'') => return Err(LexerError::UnterminatedChar(start_line, start_col)), // Empty char ''
            Some('\\') => {
                let escape_line = self.current_line; // Pos of char *after* backslash
                let escape_col = self.current_col;
                match self.consume() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'', // Allow \' within char literal
                    Some('0') => '\0',
                    Some(other) => return Err(LexerError::InvalidEscapeSequence(other, escape_line, escape_col)),
                    None => return Err(LexerError::UnterminatedChar(start_line, start_col)),
                }
            }
            Some('\n') => return Err(LexerError::UnterminatedChar(start_line, start_col)), // Newline in char literal
            Some(c) => c,
            None => return Err(LexerError::UnterminatedChar(start_line, start_col)),
        };

        // Check for closing quote
        match self.consume() {
            Some('\'') => Ok(self.token_info(Token::CharLiteral(char_val), start_line, start_col)),
            _ => Err(LexerError::UnterminatedChar(start_line, start_col)), // Missing closing quote or too many chars
        }
    }


    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexerError> {
        loop {
            // Skip whitespace
            while let Some(&c) = self.peek() {
                if c.is_whitespace() {
                    self.consume();
                } else if c == '#' { // Handle preprocessor directives like comments
                    // Skip to end of line
                    while let Some(ch) = self.consume() {
                        if ch == '\n' { break; }
                    }
                    // Loop again to skip potential leading whitespace on the *next* line
                }
                 else {
                    break; // Not whitespace or '#'
                }
            }

            // Check for comments after skipping whitespace/directives
            if self.peek() == Some(&'/') {
                // Clone iterator to peek ahead without consuming the first '/' yet
                let mut chars_clone = self.input.clone();
                chars_clone.next();

                match chars_clone.peek() {
                    Some(&'/') => { // Line comment "//"
                        self.consume(); // Consume first '/'
                        self.consume(); // Consume second '/'
                        while let Some(c) = self.consume() {
                            if c == '\n' { break; }
                        }
                        continue; // Restart loop to check for more whitespace/comments after the consumed line
                    }
                    Some(&'*') => { // Block comment "/*"
                        let comment_start_line = self.current_line;
                        let comment_start_col = self.current_col;
                        self.consume(); // Consume '/'
                        self.consume(); // Consume '*'
                        let mut maybe_end = false;
                        loop {
                            match self.consume() {
                                Some('*') => maybe_end = true,
                                Some('/') if maybe_end => break, // Found "*/"
                                Some(_) => maybe_end = false,
                                None => return Err(LexerError::UnterminatedBlockComment(comment_start_line, comment_start_col)),
                            }
                        }
                        continue; // Restart loop after consuming block comment
                    }
                    _ => break, // Just a single '/', not a comment start
                }
            } else {
                break; // Not whitespace, not a comment start
            }
        }
        Ok(())
    }


    /// Fetches the next token.
    pub fn next_token(&mut self) -> Result<TokenInfo, LexerError> {
        self.skip_whitespace_and_comments()?;

        let start_line = self.current_line;
        let start_col = self.current_col;

        // Use peek() then consume() for multi-char operators to avoid borrow errors
        if let Some(first_char) = self.consume() {
            match first_char {
                // Multi-char operators check using peek()
                '=' => {
                    if self.peek() == Some(&'=') { self.consume(); Ok(self.token_info(Token::Eq, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Assign, start_line, start_col)) }
                }
                '!' => {
                    if self.peek() == Some(&'=') { self.consume(); Ok(self.token_info(Token::Ne, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Not, start_line, start_col)) }
                }
                '<' => {
                    if self.peek() == Some(&'=') { self.consume(); Ok(self.token_info(Token::Le, start_line, start_col)) }
                    else if self.peek() == Some(&'<') { self.consume(); Ok(self.token_info(Token::Shl, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Lt, start_line, start_col)) }
                }
                '>' => {
                    if self.peek() == Some(&'=') { self.consume(); Ok(self.token_info(Token::Ge, start_line, start_col)) }
                    else if self.peek() == Some(&'>') { self.consume(); Ok(self.token_info(Token::Shr, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Gt, start_line, start_col)) }
                }
                '+' => {
                    if self.peek() == Some(&'+') { self.consume(); Ok(self.token_info(Token::Inc, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Add, start_line, start_col)) }
                }
                '-' => {
                    if self.peek() == Some(&'-') { self.consume(); Ok(self.token_info(Token::Dec, start_line, start_col)) }
                    else { Ok(self.token_info(Token::Sub, start_line, start_col)) }
                }
                '&' => {
                     // Distinguish && (Lan) from & (Ampersand/BitAnd)
                    if self.peek() == Some(&'&') { self.consume(); Ok(self.token_info(Token::Lan, start_line, start_col)) }
                     // C4 uses '&' for bitwise AND in expressions, parser needs to resolve
                    else { Ok(self.token_info(Token::Ampersand, start_line, start_col)) }
                }
                '|' => {
                    if self.peek() == Some(&'|') { self.consume(); Ok(self.token_info(Token::Lor, start_line, start_col)) }
                    else { Ok(self.token_info(Token::BitOr, start_line, start_col)) }
                }

                // Single-char operators and punctuation
                '*' => Ok(self.token_info(Token::Asterisk, start_line, start_col)),
                '/' => Ok(self.token_info(Token::Div, start_line, start_col)), // Comments handled
                '%' => Ok(self.token_info(Token::Mod, start_line, start_col)),
                '^' => Ok(self.token_info(Token::BitXor, start_line, start_col)),
                '~' => Ok(self.token_info(Token::BitNot, start_line, start_col)),
                '?' => Ok(self.token_info(Token::Cond, start_line, start_col)),
                ':' => Ok(self.token_info(Token::Colon, start_line, start_col)),
                ';' => Ok(self.token_info(Token::Semicolon, start_line, start_col)),
                ',' => Ok(self.token_info(Token::Comma, start_line, start_col)),
                '(' => Ok(self.token_info(Token::LParen, start_line, start_col)),
                ')' => Ok(self.token_info(Token::RParen, start_line, start_col)),
                '{' => Ok(self.token_info(Token::LBrace, start_line, start_col)),
                '}' => Ok(self.token_info(Token::RBrace, start_line, start_col)),
                '[' => Ok(self.token_info(Token::LBracket, start_line, start_col)),
                ']' => Ok(self.token_info(Token::RBracket, start_line, start_col)),

                // Literals
                '"' => self.read_string_literal(start_line, start_col),
                '\'' => self.read_char_literal(start_line, start_col),

                // Numbers (simple decimal for now, based on C4 lexer)
                c @ '0'..='9' => self.read_number(c, start_line, start_col),

                // Identifiers or Keywords
                c @ ('a'..='z' | 'A'..='Z' | '_') => self.read_identifier_or_keyword(c, start_line, start_col),

                // Invalid Character
                c => Err(LexerError::InvalidCharacter(c, start_line, start_col)),
            }
        } else {
             // End Of File
             Ok(self.token_info(Token::Eof, start_line, start_col))
        }
    }
}

// --- Iterator Implementation ---
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenInfo, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token_info) => {
                if token_info.token == Token::Eof {
                    None // Stop iteration after EOF token is generated
                } else {
                    Some(Ok(token_info)) // Return valid token
                }
            }
            Err(e) => Some(Err(e)), // Return lexer error
        }
    }
}