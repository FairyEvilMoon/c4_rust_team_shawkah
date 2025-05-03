//! Lexer (Tokenizer) for the C4 subset of C.
//!
//! This module takes C source code as input (a &str) and produces a stream
//! of `TokenInfo` structs, each containing a `Token` and its position
//! (line and column) in the source file. It handles C4 keywords, identifiers,
//! integer literals, character literals, string literals, operators, punctuation,
//! and skips whitespace and comments (both // and /* */).

use std::iter::Peekable;
use std::str::Chars;

// --- Token Definition (Covers C4 requirements) ---
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,

    // Keywords
    Enum, If, Else, Int, Char, Return, Sizeof, While,

    // Identifiers
    Ident(String),

    // Literals
    Number(i64), // Using i64 for flexibility, original C4 might use i32
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

    // Single char symbols / Ambiguous until parsed
    Semicolon, Comma, LParen, RParen, LBrace, RBrace, LBracket, RBracket, // ;, ,, (, ), {, }, [, ]
    /// Represents the '*' character. Ambiguous: Multiplication or Dereference.
    Asterisk,
    /// Represents the '&' character. Ambiguous: Bitwise AND or AddressOf.
    Ampersand,
}

// --- Token Information (Includes position) ---

/// Represents a token along with its position in the source code.
#[derive(Debug, Clone, PartialEq)]
pub struct TokenInfo {
    /// The specific token type.
    pub token: Token,
    /// The line number where the token starts (1-based).
    pub line: usize,
    /// The column number where the token starts (1-based).
    pub column: usize,
    // Optional: /// The starting byte position of the token in the input string.
    // pub position: usize,
}

// --- Lexer Error (Includes positions) ---

/// Represents errors that can occur during the lexing process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// An unexpected character was encountered. (Char, Line, Column)
    InvalidCharacter(char, usize, usize),
    /// A string literal was not properly terminated with a double quote. (Start Line, Start Column)
    UnterminatedString(usize, usize),
    /// A character literal was not properly terminated with a single quote. (Start Line, Start Column)
    UnterminatedChar(usize, usize),
    /// A block comment (`/*`) was not properly terminated with `*/`. (Start Line, Start Column)
    UnterminatedBlockComment(usize, usize),
    /// An invalid escape sequence (e.g., `\x`) was found within a literal. (Invalid char after '\', Line, Column)
    InvalidEscapeSequence(char, usize, usize),
    // TODO: Consider adding IntegerOverflow if parsing i64 fails for very large numbers
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::InvalidCharacter(c, l, col) => write!(f, "Invalid character '{}' at {}:{}", c, l, col),
            LexerError::UnterminatedString(l, col) => write!(f, "Unterminated string literal starting at {}:{}", l, col),
            LexerError::UnterminatedChar(l, col) => write!(f, "Unterminated character literal starting at {}:{}", l, col),
            LexerError::UnterminatedBlockComment(l, col) => write!(f, "Unterminated block comment starting at {}:{}", l, col),
            LexerError::InvalidEscapeSequence(c, l, col) => write!(f, "Invalid escape sequence '\\{}' at {}:{}", c, l, col),
        }
    }
}

impl std::error::Error for LexerError {}

// --- Lexer Implementation ---

/// The Lexer struct holds the state required for tokenizing C source code.
pub struct Lexer<'a> {
    /// Peekable iterator over the input characters.
    input: Peekable<Chars<'a>>,
    /// Current byte position in the input string.
    current_byte_pos: usize,
    /// Current line number (1-based).
    current_line: usize,
    /// Current column number (1-based).
    current_col: usize,
    /// Byte position where the current line started (used for column calculation reset).
    line_start_byte_pos: usize,
     /// Reference to the original source string (can be useful for slicing complex tokens if needed).
    #[allow(dead_code)] // Keep in case needed later
    source: &'a str,
}

impl<'a> Lexer<'a> {
    /// Creates a new Lexer for the given input string.
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            current_byte_pos: 0,
            current_line: 1,
            current_col: 1,
            line_start_byte_pos: 0,
            source: input,
        }
    }

    /// Consumes the next character from the input, updating position trackers.
    /// Returns the consumed character or None if at EOF.
    fn consume(&mut self) -> Option<char> {
        match self.input.next() {
            Some(c) => {
                self.current_byte_pos += c.len_utf8();
                if c == '\n' {
                    self.current_line += 1;
                    self.current_col = 1;
                    self.line_start_byte_pos = self.current_byte_pos;
                } else {
                    // Simple column increment for non-newline characters.
                    // Does not account for tabs expanding (typical for basic compilers).
                    self.current_col += 1;
                }
                Some(c)
            }
            None => None,
        }
    }

    /// Peeks at the next character without consuming it.
    /// Returns `Some(&char)` or `None` if at EOF.
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    /// Consumes the next character *if* it matches the expected character.
    /// Returns `true` if consumed, `false` otherwise.
    fn consume_if_eq(&mut self, expected: char) -> bool {
        if self.peek() == Some(&expected) {
            self.consume(); // Updates position
            true
        } else {
            false
        }
    }

     /// Creates a `TokenInfo` struct with the current token's details.
     fn token_info(&self, token: Token, start_line: usize, start_col: usize) -> TokenInfo {
        TokenInfo {
            token,
            line: start_line,
            column: start_col,
            // position: start_byte_pos, // optional byte position
        }
     }

    /// Reads a sequence of digits starting with `first_digit`.
    /// Assumes `first_digit` has already been consumed.
    fn read_number(&mut self, first_digit: char, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut number_str = String::new();
        number_str.push(first_digit);

        while let Some(&c) = self.peek() {
            if c.is_ascii_digit() { // Use ascii_digit for clarity
                number_str.push(self.consume().unwrap());
            } else {
                break;
            }
        }
        match number_str.parse::<i64>() {
            // C4 technically uses int (i32), but i64 provides more range.
            // Add checks here if strict i32 range is required by assignment constraints.
            Ok(num) => Ok(self.token_info(Token::Number(num), start_line, start_col)),
            Err(_) => {
                // This usually indicates overflow for i64, which is rare for typical C input.
                // A production compiler might return LexerError::IntegerOverflow here.
                 panic!(
                    "Internal Lexer Error: Failed to parse valid number string '{}' (Overflow?) at {}:{}",
                    number_str, start_line, start_col
                 )
            }
        }
    }

    /// Reads an identifier (alphanumeric + '_') or a keyword.
    /// Assumes `first_char` has already been consumed.
    fn read_identifier_or_keyword(&mut self, first_char: char, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut ident = String::new();
        ident.push(first_char);

        while let Some(&c) = self.peek() {
            // C identifiers allow alphanumeric and underscore
            if c.is_ascii_alphanumeric() || c == '_' {
                ident.push(self.consume().unwrap());
            } else {
                break;
            }
        }

        // Check if the identifier is actually a keyword
        let token = match ident.as_str() {
            "enum" => Token::Enum,
            "if" => Token::If,
            "else" => Token::Else,
            "int" => Token::Int,
            "char" => Token::Char,
            "return" => Token::Return,
            "sizeof" => Token::Sizeof,
            "while" => Token::While,
            _ => Token::Ident(ident), // Not a keyword, it's an identifier
        };
        Ok(self.token_info(token, start_line, start_col))
    }

    /// Reads a string literal enclosed in double quotes. Handles basic C escapes.
    /// Assumes the opening `"` has already been consumed.
    fn read_string_literal(&mut self, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let mut literal = String::new();
        loop {
            // Capture position before consuming potentially problematic char (like '\' or '"')
            // FIX: Prefix with underscore as these are only used for potential immediate errors below
            let _current_line = self.current_line; // Prefixed
            let _current_col = self.current_col;  // Prefixed

            match self.consume() {
                Some('"') => {
                    // End of string literal
                    return Ok(self.token_info(Token::StringLiteral(literal), start_line, start_col));
                }
                Some('\\') => {
                    // Handle escape sequence
                    // We need the position *after* the backslash for accurate error reporting
                    let escape_char_line = self.current_line;
                    let escape_char_col = self.current_col;
                    match self.consume() {
                        Some('n') => literal.push('\n'),
                        Some('t') => literal.push('\t'),
                        Some('"') => literal.push('"'),
                        Some('\\') => literal.push('\\'),
                        Some('0') => literal.push('\0'),
                        // Other C escapes like \r, \a, \b, \f, \v are less common in simple subsets
                        Some(other) => {
                            // Use the position of the character *following* the backslash
                            return Err(LexerError::InvalidEscapeSequence(other, escape_char_line, escape_char_col));
                        }
                        None => {
                            // EOF right after backslash
                            return Err(LexerError::UnterminatedString(start_line, start_col));
                        }
                    }
                }
                Some(c) => {
                    if c == '\n' {
                         // Unescaped newline inside string literal is usually an error in C.
                         // Report the error starting from the beginning of the literal.
                         // Using _current_line/_current_col here would point to the newline itself,
                         // but usually the error points to the start.
                         return Err(LexerError::UnterminatedString(start_line, start_col));
                    }
                    literal.push(c);
                }
                None => {
                    // Reached EOF before finding closing quote
                    return Err(LexerError::UnterminatedString(start_line, start_col));
                }
            }
        }
    }

    /// Reads a character literal enclosed in single quotes. Handles basic C escapes.
    /// Assumes the opening `'` has already been consumed.
    fn read_char_literal(&mut self, start_line: usize, start_col: usize) -> Result<TokenInfo, LexerError> {
        let char_val = match self.consume() {
            Some('\\') => { // Handle escape sequence
                let escape_line = self.current_line; // Position of the char *after* backslash
                let escape_col = self.current_col;
                match self.consume() {
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('\'') => '\'',
                    Some('\\') => '\\',
                    Some('0') => '\0',
                    // Other C escapes...
                    Some(other) => {
                        return Err(LexerError::InvalidEscapeSequence(other, escape_line, escape_col));
                    }
                    None => {
                        // EOF right after backslash
                        return Err(LexerError::UnterminatedChar(start_line, start_col));
                    }
                }
            }
            Some('\'') => {
                // Empty char literal '' is invalid. We consumed the first ', now saw the second one immediately.
                return Err(LexerError::UnterminatedChar(start_line, start_col)); // Or a more specific error?
            }
            Some(c) => {
                 if c == '\n' {
                     // Newline in char literal is invalid
                     return Err(LexerError::UnterminatedChar(start_line, start_col));
                 }
                 c
            }
            None => {
                // Reached EOF before finding char or closing quote
                return Err(LexerError::UnterminatedChar(start_line, start_col));
            }
        };

        // Expect the closing single quote
        if !self.consume_if_eq('\'') {
            // Found the character (or escape) but not the closing quote
            return Err(LexerError::UnterminatedChar(start_line, start_col));
        }

        Ok(self.token_info(Token::CharLiteral(char_val), start_line, start_col))
    }

    /// Consumes and ignores whitespace characters.
    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c.is_whitespace() {
                self.consume();
            } else {
                break;
            }
        }
    }

    // Note: Comment skipping is handled directly within `next_token` for robustness.
    // A separate `skip_comments` function can be tricky if not carefully implemented
    // to avoid consuming characters that might belong to other tokens (like '/').

    /// Fetches the next token from the input stream.
    ///
    /// This is the main entry point for tokenization after creating the Lexer.
    /// It skips whitespace and comments and returns the next valid `TokenInfo`
    /// or a `LexerError`. Returns `Ok(TokenInfo { token: Token::Eof, .. })`
    /// exactly once when the end of the input is reached.
    pub fn next_token(&mut self) -> Result<TokenInfo, LexerError> {
        loop {
            // --- 1. Skip Whitespace ---
            self.skip_whitespace();

            // --- 2. Skip Comments (Handle // and /* */) ---
            // We peek ahead to distinguish comments from division operator '/'
            if self.peek() == Some(&'/') {
                // Need to look at the *next* character without consuming the first '/' yet
                let mut chars_clone = self.input.clone(); // Clone iterator for peeking
                chars_clone.next(); // Advance the clone past the first '/'

                match chars_clone.peek() {
                    Some(&'/') => {
                        // Found '//', consume the line comment
                        self.consume(); // Consume the first '/'
                        self.consume(); // Consume the second '/'
                        while let Some(c) = self.consume() {
                            if c == '\n' { break; } // Consume until newline or EOF
                        }
                        continue; // Restart the loop to find the next token
                    }
                    Some(&'*') => {
                        // Found '/*', consume the block comment
                        let comment_start_line = self.current_line; // For error reporting
                        let comment_start_col = self.current_col;
                        self.consume(); // Consume the '/'
                        self.consume(); // Consume the '*'

                        let mut maybe_end = false; // State: Did we just see a '*'?
                         loop {
                             match self.consume() {
                                 Some('*') => maybe_end = true,
                                 Some('/') if maybe_end => break, // Found '*/', end of comment
                                 Some(_) => maybe_end = false,    // Any other char resets state
                                 None => {
                                     // Reached EOF inside block comment
                                     return Err(LexerError::UnterminatedBlockComment(comment_start_line, comment_start_col));
                                 }
                             }
                         }
                         continue; // Restart the loop to find the next token
                    }
                    _ => {
                        // It's just a single '/', which will be handled as Division below.
                        // Do nothing here, let the main token logic handle it.
                    }
                }
            } // End of comment handling ('/')

            // --- 3. Record Token Start Position (after skipping) ---
            let token_start_line = self.current_line;
            let token_start_col = self.current_col;
            // let token_start_byte_pos = self.current_byte_pos; // Optional

            // --- 4. Recognize the Next Token ---
            match self.consume() {
                // End Of File
                None => return Ok(self.token_info(Token::Eof, token_start_line, token_start_col)),

                // Multi-character operators (must check before single-char versions)
                Some('=') => {
                    let token = if self.consume_if_eq('=') { Token::Eq } else { Token::Assign };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                Some('!') => {
                    let token = if self.consume_if_eq('=') { Token::Ne } else { Token::Not };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                Some('<') => {
                    let token = if self.consume_if_eq('=') { Token::Le }
                                else if self.consume_if_eq('<') { Token::Shl }
                                else { Token::Lt };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                 Some('>') => {
                    let token = if self.consume_if_eq('=') { Token::Ge }
                                else if self.consume_if_eq('>') { Token::Shr }
                                else { Token::Gt };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                 }
                Some('+') => {
                    let token = if self.consume_if_eq('+') { Token::Inc } else { Token::Add };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                Some('-') => {
                    let token = if self.consume_if_eq('-') { Token::Dec } else { Token::Sub };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                Some('&') => {
                    // Distinguishes '&' (BitAnd/AddressOf) from '&&' (LogAnd)
                    let token = if self.consume_if_eq('&') { Token::LogAnd } else { Token::Ampersand };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }
                Some('|') => {
                    // Distinguishes '|' (BitOr) from '||' (LogOr)
                    let token = if self.consume_if_eq('|') { Token::LogOr } else { Token::BitOr };
                    return Ok(self.token_info(token, token_start_line, token_start_col));
                }

                // Single-character operators and punctuation
                Some('*') => return Ok(self.token_info(Token::Asterisk, token_start_line, token_start_col)), // Ambiguous (Mul/Deref)
                Some('/') => return Ok(self.token_info(Token::Div, token_start_line, token_start_col)), // Comments handled earlier
                Some('%') => return Ok(self.token_info(Token::Mod, token_start_line, token_start_col)),
                Some('^') => return Ok(self.token_info(Token::BitXor, token_start_line, token_start_col)),
                Some(';') => return Ok(self.token_info(Token::Semicolon, token_start_line, token_start_col)),
                Some(',') => return Ok(self.token_info(Token::Comma, token_start_line, token_start_col)),
                Some('(') => return Ok(self.token_info(Token::LParen, token_start_line, token_start_col)),
                Some(')') => return Ok(self.token_info(Token::RParen, token_start_line, token_start_col)),
                Some('{') => return Ok(self.token_info(Token::LBrace, token_start_line, token_start_col)),
                Some('}') => return Ok(self.token_info(Token::RBrace, token_start_line, token_start_col)),
                Some('[') => return Ok(self.token_info(Token::LBracket, token_start_line, token_start_col)),
                Some(']') => return Ok(self.token_info(Token::RBracket, token_start_line, token_start_col)),

                // Literals
                Some('"') => return self.read_string_literal(token_start_line, token_start_col),
                Some('\'') => return self.read_char_literal(token_start_line, token_start_col),

                // Numbers (start with a digit)
                Some(c @ '0'..='9') => return self.read_number(c, token_start_line, token_start_col),

                // Identifiers or Keywords (start with letter or underscore)
                Some(c @ ('a'..='z' | 'A'..='Z' | '_')) => return self.read_identifier_or_keyword(c, token_start_line, token_start_col),

                // --- Error Case ---
                Some(c) => {
                    // Any other character is considered invalid in C4's subset
                    return Err(LexerError::InvalidCharacter(c, token_start_line, token_start_col));
                }
           }
        } // End loop
    }

    /// Helper function to tokenize the entire input string at once.
    /// Useful mainly for testing or simple use cases.
    /// Returns a `Vec<TokenInfo>` on success (including EOF) or the first `LexerError`.
    #[allow(dead_code)]
    pub fn tokenize_all(&mut self) -> Result<Vec<TokenInfo>, LexerError> {
        let mut tokens = Vec::new();
        loop {
            let token_info = self.next_token()?;
            let is_eof = token_info.token == Token::Eof;
            tokens.push(token_info);
            if is_eof {
                break; // Stop after adding the EOF token
            }
        }
        Ok(tokens)
    }
}

// --- Iterator Implementation ---
// Allows using the lexer in `for` loops: `for result in Lexer::new(source) { ... }`
// The iterator yields `Result<TokenInfo, LexerError>` and stops after yielding EOF once
// (by returning `None` after the EOF token has been successfully processed).
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenInfo, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token_info) => {
                if token_info.token == Token::Eof {
                    // We've successfully produced the EOF token, now stop the iteration.
                    None
                } else {
                    // Return the valid token info.
                    Some(Ok(token_info))
                }
            }
            Err(e) => {
                // Propagate the error, stopping iteration implicitly on error.
                 Some(Err(e))
            }
        }
    }
}