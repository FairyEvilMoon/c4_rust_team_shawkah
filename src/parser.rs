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