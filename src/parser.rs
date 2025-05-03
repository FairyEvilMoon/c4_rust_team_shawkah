//! Minimal Parser for C4 subset, focusing on 'hello world'.
//! Directly emits VM instructions.

use crate::lexer::{Lexer, Token, TokenInfo, LexerError};
// Removed unused SymbolEntry import
use crate::symbol::{SymbolTable, SymbolClass, DataType};
use crate::vm::{Instruction, Value}; // Assuming VM instructions are i32
use std::iter::Peekable;

// --- Parser Error ---
// Removed Eq derive because Token contains String which is not Eq
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    LexerError(LexerError),
    UnexpectedToken {
        expected: String, // Description of expected token(s)
        found: Token,
        line: usize,
        column: usize,
    },
    UndefinedSymbol(String, usize, usize), // Name, line, col
    NotAFunction(String, usize, usize),    // Name, line, col
    EndOfInput, // Reached end of input unexpectedly
    ExpectedEOF, // Expected EOF but found more tokens
    Other(String), // Generic error
}

// Conversion from LexerError to ParseError
impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::LexerError(e) => write!(f, "Lexer error: {}", e),
            ParseError::UnexpectedToken { expected, found, line, column } => {
                write!(f, "Parse error at {}:{}: Expected {}, found {:?}", line, column, expected, found)
            }
            ParseError::UndefinedSymbol(name, line, col) => {
                write!(f, "Parse error at {}:{}: Undefined symbol '{}'", line, col, name)
            }
             ParseError::NotAFunction(name, line, col) => {
                write!(f, "Parse error at {}:{}: '{}' is not a function", line, col, name)
            }
            ParseError::EndOfInput => write!(f, "Parse error: Unexpected end of input"),
            ParseError::ExpectedEOF => write!(f, "Parse error: Expected end of file but found more tokens"),
            ParseError::Other(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for ParseError {}

// --- Parser Implementation ---

// Type alias for convenience
type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    symbol_table: SymbolTable,
    code: Vec<Value>,      // Bytecode for the VM
    data_segment: Vec<u8>, // To store string literals
    current_token_info: Option<TokenInfo>, // Stores the *last consumed* token info for error reporting
    entry_point: Option<Value>, // Address of the main function
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut symbol_table = SymbolTable::new();

        // Pre-register built-in functions like printf
        // Using PRINTF_SYSCALL_ADDR (-1) as a special value for the VM hook.
        const PRINTF_SYSCALL_ADDR: i64 = -1; // Needs to be i64 for symbol table
        symbol_table.add(
            "printf".to_string(),
            Token::Ident("printf".to_string()), // Placeholder token
            SymbolClass::Sys, // System call/built-in
            DataType::Int, // Standard C printf returns int
            PRINTF_SYSCALL_ADDR, // Special value indicating built-in/syscall
        ).expect("Failed to add built-in printf");

        Parser {
            tokens: lexer.peekable(),
            symbol_table,
            code: Vec::new(),
            data_segment: Vec::new(),
            current_token_info: None,
            entry_point: None,
        }
    }

    /// Consumes the next token from the lexer.
    /// Returns the TokenInfo or a ParseError.
    /// Updates `current_token_info` for error context.
    /// Returns EndOfInput error ONLY if the underlying lexer iterator returns None *before* EOF token.
    /// Successfully consuming the EOF token is handled by the caller checking token_info.token.
    fn next_token(&mut self) -> ParseResult<TokenInfo> {
        match self.tokens.next() {
            Some(Ok(token_info)) => {
                self.current_token_info = Some(token_info.clone());
                Ok(token_info) // Return the token info (could be EOF)
            }
            Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err)),
            None => {
                 // This path means the Lexer iterator finished without yielding EOF explicitly.
                 // This *shouldn't* happen with the current Lexer impl, which yields EOF once.
                 // If it does, treat it as unexpected end.
                 Err(ParseError::EndOfInput)
            }
        }
    }

     /// Peeks at the next token without consuming it.
     /// Returns Ok(Some(&Token)) if a token is available (could be EOF).
     /// Returns Ok(None) if the stream is exhausted *after* EOF was yielded.
     /// Returns Err(ParseError) if lexing fails.
     fn peek_token(&mut self) -> ParseResult<Option<&Token>> {
         match self.tokens.peek() {
             Some(Ok(token_info)) => Ok(Some(&token_info.token)), // Return ref to token
             Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err.clone())), // Clone error
             None => Ok(None), // Iterator end (after EOF token was processed)
         }
     }

    /// Gets the position (line, col) from the last consumed token, or default (0,0).
    fn current_pos(&self) -> (usize, usize) {
        self.current_token_info.as_ref().map_or((0, 0), |ti| (ti.line, ti.column))
    }

     /// Expects a specific token, consumes it if matches, otherwise returns error.
     /// Handles EOF correctly - if EOF is expected, it succeeds.
     fn expect_token(&mut self, expected_token: Token, expected_desc: &str) -> ParseResult<TokenInfo> {
         let token_info = self.next_token()?; // Consumes the next token
         if token_info.token == expected_token {
             Ok(token_info)
         } else {
            // If next_token returned EOF but it wasn't expected
            if token_info.token == Token::Eof {
                return Err(ParseError::EndOfInput); // Unexpected end of input
            }
             Err(ParseError::UnexpectedToken {
                 expected: expected_desc.to_string(),
                 found: token_info.token,
                 line: token_info.line,
                 column: token_info.column,
             })
         }
     }

    /// Expects an identifier token, consumes it, and returns its name.
    fn expect_identifier(&mut self, expected_desc: &str) -> ParseResult<(String, TokenInfo)> {
        let token_info = self.next_token()?;
        if let Token::Ident(name) = token_info.token.clone() { // Clone name
            Ok((name, token_info))
        } else {
             // Handle EOF case more specifically if needed
            if token_info.token == Token::Eof {
                 return Err(ParseError::EndOfInput);
             }
            Err(ParseError::UnexpectedToken {
                expected: expected_desc.to_string(),
                found: token_info.token,
                line: token_info.line,
                column: token_info.column,
            })
        }
    }


    /// Emits a single VM instruction (opcode only).
    fn emit(&mut self, instruction: Instruction) {
        self.code.push(instruction as Value);
    }

    /// Emits a VM instruction followed by its operand.
    fn emit_with_operand(&mut self, instruction: Instruction, operand: Value) {
        self.code.push(instruction as Value);
        self.code.push(operand);
    }

    /// Adds a string literal to the data segment and returns its address (index).
    /// Note: VM addresses are Value (i32), data segment indices are usize. Ensure compatibility.
    fn add_string_literal(&mut self, literal: &str) -> Value {
        let address = self.data_segment.len();
        self.data_segment.extend_from_slice(literal.as_bytes());
        self.data_segment.push(0); // Null-terminate the string in the data segment

        // Important: Check if the address fits within Value (i32) range
        if address > i32::MAX as usize {
            // This should not happen in reasonable programs, but good practice
            panic!("Data segment address exceeds i32::MAX!");
        }
        address as Value // Return the starting address as i32
    }

    // --- Parsing Rules ---

    /// Parses the entire program (currently just one function: main).
/// Parses the entire program (currently just one function: main).
pub fn parse_program(mut self) -> ParseResult<(Vec<Value>, Vec<u8>, Value)> {
    // Define preamble instructions SIZE
    const PREAMBLE_SIZE: usize = 3; // CALL, ADDR, EXIT

    // --- Parse main function FIRST ---
    // This populates self.code with instructions for main, starting at index 0 WITHIN self.code.
    // It also populates self.symbol_table with main's relative address (0).
    self.parse_global_declaration()?;

    // --- Check for end of input AFTER main is parsed ---
    match self.peek_token() {
         Ok(None) => { /* Expected state: stream exhausted */ }
         Ok(Some(_token)) => { // Use _token to silence unused warning
             // Found unexpected tokens after main function definition
             match self.next_token() {
                 Ok(unexpected_token_info) => {
                     return Err(ParseError::UnexpectedToken {
                         expected: "end of file".to_string(),
                         found: unexpected_token_info.token,
                         line: unexpected_token_info.line,
                         column: unexpected_token_info.column,
                     });
                 }
                 Err(e) => return Err(e), // Error consuming the unexpected token
             }
         }
         Err(e) => return Err(e), // Lexer error during peek
    }

    // --- Assemble Final Code ---
    // The actual entry point address for the CALL instruction is AFTER the preamble.
    let actual_main_address: Value = PREAMBLE_SIZE as Value;

    // Create the preamble vector using the correct address
    let preamble = vec![
        Instruction::Call as Value,
        actual_main_address, // Address where main's code starts (index 3)
        Instruction::Exit as Value,
    ];

    // Concatenate preamble and the code generated for main's body
    // IMPORTANT CAVEAT: If the code in self.code contained any jumps or address calculations
    // (like JMP, JZ, CALL other_func, LEA with labels), those addresses would be relative
    // to the start of self.code (offset 0). Prepending the preamble means these internal
    // addresses would need to be "relocated" (adjusted by PREAMBLE_SIZE).
    // For hello.c, this is not an issue as it only calls the external printf.
    let final_code = [preamble, self.code].concat();

    // Return the final code, data, and the *actual* entry point address.
    // While the preamble handles the initial jump, returning the correct start address
    // might be useful for debugging or future VM features.
    Ok((final_code, self.data_segment, actual_main_address))
}

// Modify parse_global_declaration to ONLY store the relative address (0) for main
// and NOT set the parser's temporary entry_point field.
fn parse_global_declaration(&mut self) -> ParseResult<()> {
    self.expect_token(Token::Int, "type (int)")?;
    let (name, name_info) = self.expect_identifier("function name")?;

    if name != "main" {
         return Err(ParseError::Other(format!("Expected 'main' function, found '{}' at {}:{}", name, name_info.line, name_info.column)));
    }

    self.expect_token(Token::LParen, "'('")?;
    self.expect_token(Token::RParen, "')'")?;
    self.expect_token(Token::LBrace, "'{'")?;

    // Store the address of main RELATIVE to the start of its code block (always 0 here).
    // No need to store absolute final address yet.
    let main_relative_address: i64 = 0;

    // Remove: self.entry_point = Some(main_address); // Don't set this here

     self.symbol_table.add(
        name.clone(),
        Token::Ident(name),
        SymbolClass::Fun,
        DataType::Int,
        main_relative_address, // Store relative address 0 for the symbol 'main'
    ).map_err(|e| ParseError::Other(e))?;


    // --- Function Body ---
    // Code emitted here starts at index 0 *within self.code*
    self.emit_with_operand(Instruction::Ent, 0); // Emit main's entry code

    // Parse statements loop...
    loop {
         match self.peek_token()? {
             Some(Token::RBrace) => {
                 self.next_token()?; // Consume '}'
                 break; // End of function body
             }
             Some(Token::Eof) | None => {
                let (line, col) = self.current_pos();
                let found_token = if self.peek_token()? == Some(&Token::Eof) { Token::Eof } else { Token::Eof };
                return Err(ParseError::UnexpectedToken {
                    expected: "'}' or statement".to_string(),
                    found: found_token,
                    line,
                    column: col,
                });
             }
             _ => {
                self.parse_statement()?;
             }
         }
    }
    // ... end of loop ...

    Ok(())
}


    /// Parses a single statement.
    fn parse_statement(&mut self) -> ParseResult<()> {
        // Peek first to decide what kind of statement it is
        match self.peek_token()? {
            Some(Token::Return) => {
                self.next_token()?; // Consume 'return'
                self.parse_expression()?; // Evaluate expression, result ends up in AX
                self.emit(Instruction::Lev); // Leave function (restores BP, pops PC)
                self.expect_token(Token::Semicolon, "';'")?;
            }
            Some(Token::Eof) | None => return Err(ParseError::EndOfInput), // Unexpected EOF
            // Handle other statements here if needed (e.g., if, while, '{' for block)
            _ => {
                // Assume expression statement (like a function call)
                self.parse_expression()?;
                 // Result of expression statement is often discarded.
                 // In C, the value remains (e.g., in AX), but isn't used unless assigned.
                 // For printf("hello"), the return value of printf stays in AX.
                 // C4 VM doesn't seem to have explicit POP/discard.
                self.expect_token(Token::Semicolon, "';'")?;
            }
        }
        Ok(())
    }

    /// Parses an expression (highly simplified).
    fn parse_expression(&mut self) -> ParseResult<()> {
        // Only handles: Number literal, String literal, Function call (printf)
         let token_info = self.next_token()?;
         match token_info.token {
             Token::Number(val) => {
                 // Check if number fits in i32
                 if val < i32::MIN as i64 || val > i32::MAX as i64 {
                     return Err(ParseError::Other(format!("Integer literal {} out of i32 range at {}:{}", val, token_info.line, token_info.column)));
                 }
                 self.emit_with_operand(Instruction::Imm, val as Value); // Load number into AX
             }
             Token::StringLiteral(s) => {
                 let addr = self.add_string_literal(&s);
                 self.emit_with_operand(Instruction::Imm, addr); // Load string address (Value/i32) into AX
             }
             Token::Ident(name) => {
                // Look ahead to see if it's a function call
                 if self.peek_token()? == Some(&Token::LParen) {
                     self.next_token()?; // Consume '('
                     self.parse_function_call(name, token_info.line, token_info.column)?;
                 } else {
                     // Variable access - not supported yet in this minimal parser
                     // Need symbol table lookup here for variables eventually.
                     return Err(ParseError::Other(format!("Variable access for '{}' not implemented at {}:{}", name, token_info.line, token_info.column)));
                 }
             }
             Token::Eof => return Err(ParseError::EndOfInput), // Unexpected EOF
            _ => {
                 return Err(ParseError::UnexpectedToken {
                     expected: "expression (number, string, or function call)".to_string(),
                     found: token_info.token,
                     line: token_info.line,
                     column: token_info.column,
                 });
             }
         }
         Ok(())
    }

     /// Parses a function call like `name(...)`
    fn parse_function_call(&mut self, name: String, line: usize, column: usize) -> ParseResult<()> {
        // Find function in symbol table
        let func_entry = self.symbol_table.find(&name).cloned() // Clone to avoid borrow issues
            .ok_or_else(|| ParseError::UndefinedSymbol(name.clone(), line, column))?;

        // Ensure it's a callable symbol (Function or System call)
        let func_addr_i64 = match func_entry.class { // Address from symbol table is i64
            SymbolClass::Fun | SymbolClass::Sys => func_entry.value,
            _ => return Err(ParseError::NotAFunction(name, line, column)),
        };

        let mut arg_count = 0;
        // Parse arguments (pushing them onto stack)
        if self.peek_token()? != Some(&Token::RParen) {
             loop {
                 self.parse_expression()?; // Evaluate argument expression (result in AX)
                 self.emit(Instruction::Push); // Push argument (AX) onto stack
                 arg_count += 1;

                 // Check for comma or closing paren
                 match self.peek_token()? {
                    Some(Token::Comma) => {
                        self.next_token()?; // Consume ','
                        // Continue loop for next argument
                    }
                    Some(Token::RParen) => {
                        break; // End of arguments
                    }
                    Some(Token::Eof) | None => return Err(ParseError::EndOfInput),
                    Some(other) => return Err(ParseError::UnexpectedToken {
                        expected: "',' or ')'".to_string(),
                        found: other.clone(),
                        // Get position from peeked token
                        line: self.tokens.peek().unwrap().as_ref().ok().map_or(line, |ti| ti.line),
                        column: self.tokens.peek().unwrap().as_ref().ok().map_or(column, |ti| ti.column),
                    }),
                 }
             }
         }

        // We must find the closing parenthesis now
        self.expect_token(Token::RParen, "')'")?;

         // Check if the address fits in Value (i32) before casting
         // (except for the special -1 case)
         if func_addr_i64 != -1 && (func_addr_i64 < i32::MIN as i64 || func_addr_i64 > i32::MAX as i64) {
             return Err(ParseError::Other(format!("Function address {} for '{}' out of i32 range", func_addr_i64, name)));
         }
         let func_addr_val = func_addr_i64 as Value; // Cast i64 address to Value (i32)

        // Emit call instruction (using the Value/i32 address)
        self.emit_with_operand(Instruction::Call, func_addr_val);


        // Clean up arguments from stack if caller cleans (C convention)
        if arg_count > 0 {
             // FIX: Cast arg_count (usize) to Value (i32)
             if arg_count > i32::MAX as usize {
                return Err(ParseError::Other(format!("Argument count {} exceeds i32::MAX", arg_count)));
             }
             self.emit_with_operand(Instruction::Adj, arg_count as Value);
        }

        // Result of function call is now in AX (e.g., return value of printf)

        Ok(())
    }
}