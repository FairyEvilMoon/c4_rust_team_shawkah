//! Parser for the C4 language subset.

use crate::lexer::{Lexer, Token, LexerError};
use crate::vm::Instruction;
use crate::symbol::{SymbolTable, SymbolEntry, SymbolClass, DataType};
use std::iter::Peekable;
use std::mem; // For mem::discriminant

// Operator Precedence Levels (Matching C4's enum order)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    P0 Lowest = 0, // Sentinel
    Assign,     // =
    Cond,       // ?:
    LogOr,      // ||
    LogAnd,     // &&
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Eq, Ne,     // ==, !=
    Lt, Gt, Le, Ge, // <, >, <=, >=
    Shl, Shr,   // <<, >>
    Add, Sub,   // +, -
    Mul, Div, Mod, // *, /, %
    Unary,     // ++, -- (prefix), !, ~, *(deref), &(addr), sizeof
    Postfix,    // ++, -- (postfix), [], () (call)
    Primary,    // Literals, identifiers, (...)
}

// Map tokens to precedence - based on C4 `expr` logic
fn token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Assign => Precedence::Assign,
        // Token::Cond => Precedence::Cond, 
        // ?: handled specially
        Token::LogOr => Precedence::LogOr,
        Token::LogAnd => Precedence::LogAnd,
        Token::BitOr => Precedence::BitOr,
        Token::BitXor => Precedence::BitXor,
        Token::Ampersand | Token::BitAnd => Precedence::BitAnd, // Need context for &
        Token::Eq => Precedence::Eq, Token::Ne => Precedence::Ne,
        Token::Lt => Precedence::Lt, Token::Gt => Precedence::Gt, Token::Le => Precedence::Le, Token::Ge => Precedence::Ge,
        Token::Shl => Precedence::Shl, Token::Shr => Precedence::Shr,
        Token::Add => Precedence::Add, Token::Sub => Precedence::Sub,
        Token::Asterisk | Token::Mul => Precedence::Mul, // Need context for *
        Token::Div => Precedence::Mul, Token::Mod => Precedence::Mul,
        Token::Inc | Token::Dec => Precedence::Unary, // Or Postfix depending on context
        Token::LBracket => Precedence::Postfix, // Array subscript
        Token::LParen => Precedence::Postfix, // Function call or grouping
        _ => Precedence::P0 Lowest,
    }

}

// Define Parser errors
#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    LexerError(LexerError),
    UnexpectedToken(Token, String),
    UnexpectedEOF,
    UndefinedSymbol(String),
    Redefinition(String),
    NotImplemented(String),
    TypeError(String),
    SyntaxError(String),
    InternalError(String), // For unexpected internal states
}

// Convert LexerError to ParserError
impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> Self {
        ParserError::LexerError(err)
    }
}

// Parser structure
pub struct Parser<'a> {
    lexer: Peekable<lexer<'a>>,
    current_token: Token,
    code: Vec<i64>, // Vector of instructions
    symbols: SymbolTable,
    data_segment: Vec<u8>, // For string literals, etc.
    main_entry_point: Option<usize>, // Entry point for main function

    // Scope information
    local_offset: usize, // Offset for local variables
}
impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Result<Self, ParserError> {
        let first_token = lexer.next_token()?;
        Ok(Parser {
            lexer: lexer.peekable(),
            current_token: first_token,
            code: Vec::new(),
            symbols: SymbolTable::new(),
            data_segment: Vec::new(),
            main_entry_point: None,
            local_offset: 0,
        })
    }

    // Token Handling
    fn consume(&mut self) -> Result<(), ParserError> {
        if self.current_token == Token::EOF {
            return Err(ParserError::UnexpectedEOF);
        }
        // Advance the lexer before updating the current token
        let next = self.lexer.next().unwrap_or(Ok(Token::EOF))?;
        self.current_token = next;
        Ok(())
    }

    // Check if the current token matches the expected token
    fn expect(&mut self, expected: Token) -> Result<(), ParserError> {
        // Use mem::discriminant to compare enum variants without comparing contained values
        if mem::discriminant(&self.current_token) == mem::discriminant(&expected) {
            self.consume()
        } else {
            let description = format!("{:?}", expected);
            Err(ParserError::UnexpectedToken(self.current_token.clone(), description))
        }
    }

    // Check if the current token matches
    fn check(&self, expected: &Token) -> bool {
        mem::discriminant(&self.current_token) == mem::discriminant(&expected)
    }

    // Peeks at the next token without consuming the current one
    fn peek(&mut self) -> Result<&Token, ParserError> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token),
            Some(Err(err)) => Err(ParserError::LexerError(err.clone())),
            None => Ok(&Token::EOF),
        }
    }
}