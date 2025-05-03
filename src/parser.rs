//! Parser for a larger C4 subset, handling local variables, assignments, and basic expressions.
//! Directly emits VM instructions.

use crate::lexer::{Lexer, Token, TokenInfo, LexerError};
use crate::symbol::{SymbolTable, SymbolClass, DataType};
use crate::vm::{Instruction, Value};
use std::iter::Peekable;
use std::mem; // Required for mem::swap

// --- Parser Error ---
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
    NotAVariable(String, usize, usize),    // Name, line, col (For assignments/usage)
    NotAssignable(String, usize, usize), // Trying to assign to non-lvalue
    EndOfInput, // Reached end of input unexpectedly (used internally by next_token)
    Other(String), // Generic error
    Redeclaration(String, usize, usize), // For variables/functions in the same scope
}

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
                // Handle EOF display nicely
                let found_str = if *found == Token::Eof { "end of file".to_string() } else { format!("{:?}", found) };
                write!(f, "Parse error at {}:{}: Expected {}, found {}", line, column, expected, found_str)
            }
            ParseError::UndefinedSymbol(name, line, col) => {
                write!(f, "Parse error at {}:{}: Undefined symbol '{}'", line, col, name)
            }
             ParseError::NotAFunction(name, line, col) => {
                write!(f, "Parse error at {}:{}: '{}' is not a function", line, col, name)
            }
            ParseError::NotAVariable(name, line, col) => {
                write!(f, "Parse error at {}:{}: '{}' is not a variable or parameter", line, col, name)
            }
            ParseError::NotAssignable(name, line, col) => {
                write!(f, "Parse error at {}:{}: Cannot assign to '{}' (not an l-value)", line, col, name)
            }
            ParseError::Redeclaration(name, line, col) => {
                 write!(f, "Parse error at {}:{}: Redeclaration of symbol '{}'", line, col, name)
            }
            ParseError::EndOfInput => write!(f, "Parse error: Unexpected end of input"),
            ParseError::Other(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for ParseError {}


// --- Parser Implementation ---

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
    symbol_table: SymbolTable,
    code: Vec<Value>, // Main code buffer
    data_segment: Vec<u8>,
    current_token_info: Option<TokenInfo>, // Stores the *last consumed* token info
    // Function-specific state:
    current_func_locals_size: usize, // Track size of locals for current function (in words/Values)
    ent_patch_location: Option<usize>, // Index in `code` where ENT operand needs patching
}

// Define constant at the implementation level so it's accessible to methods
const PRINTF_SYSCALL_ADDR: i64 = -1;

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut symbol_table = SymbolTable::new();

        // Add built-in functions like printf
        symbol_table.add(
            "printf".to_string(),
            Token::Ident("printf".to_string()), // Token is somewhat arbitrary here
            SymbolClass::Sys,
            DataType::Int, // Standard C printf returns int
            PRINTF_SYSCALL_ADDR,
        ).expect("Failed to add built-in printf");

        Parser {
            tokens: lexer.peekable(),
            symbol_table,
            code: Vec::new(),
            data_segment: Vec::new(),
            current_token_info: None,
            current_func_locals_size: 0,
            ent_patch_location: None,
        }
    }

    // --- Token Helpers ---

    /// Consumes the next token from the lexer.
    /// Returns the TokenInfo or a ParseError (LexerError or EndOfInput).
    /// Updates `current_token_info`.
    #[inline(always)]
    fn next_token(&mut self) -> ParseResult<TokenInfo> {
         match self.tokens.next() {
            Some(Ok(token_info)) => {
                self.current_token_info = Some(token_info.clone());
                Ok(token_info) // Return the token info (could be EOF)
            }
            Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err)),
            None => {
                 // Iterator finished without yielding EOF explicitly. Treat as unexpected end.
                 Err(ParseError::EndOfInput)
            }
        }
    }

    /// Peeks at the next token without consuming it.
    /// Returns Ok(Some(&Token)) if a token is available (could be EOF).
    /// Returns Ok(None) if the stream is exhausted *after* EOF was yielded or prematurely.
    /// Returns Err(ParseError::LexerError) if lexing fails during peek.
    #[inline(always)]
     fn peek_token(&mut self) -> ParseResult<Option<&Token>> {
         match self.tokens.peek() {
             Some(Ok(token_info)) => Ok(Some(&token_info.token)), // Return ref to token
             Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err.clone())), // Clone error
             None => Ok(None), // Iterator end (after EOF token was processed or stream empty)
         }
     }

     /// Gets the position (line, col) from the last consumed token, or default (0,0).
    #[inline(always)]
    fn current_pos(&self) -> (usize, usize) {
        self.current_token_info.as_ref().map_or((0, 0), |ti| (ti.line, ti.column))
    }

     /// Expects a specific token, consumes it if matches, otherwise returns error.
    #[inline(always)]
     fn expect_token(&mut self, expected_token: Token, expected_desc: &str) -> ParseResult<TokenInfo> {
         let token_info = self.next_token()?; // Consumes the next token or returns EndOfInput/LexerError
         if token_info.token == expected_token {
             Ok(token_info)
         } else {
             // Create error using the position info from the *actually found* token
             Err(ParseError::UnexpectedToken {
                 expected: expected_desc.to_string(),
                 found: token_info.token, // The token we actually got
                 line: token_info.line,
                 column: token_info.column,
             })
         }
     }

    /// Expects an identifier token, consumes it, and returns its name and info.
    #[inline(always)]
    fn expect_identifier(&mut self, expected_desc: &str) -> ParseResult<(String, TokenInfo)> {
        let token_info = self.next_token()?;
        if let Token::Ident(name) = token_info.token.clone() { // Clone name
            Ok((name, token_info))
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected_desc.to_string(),
                found: token_info.token,
                line: token_info.line,
                column: token_info.column,
            })
        }
    }


    // --- Code Emission ---
    #[inline(always)]
    fn emit(&mut self, instruction: Instruction) {
        self.code.push(instruction as Value);
    }
    #[inline(always)]
    fn emit_with_operand(&mut self, instruction: Instruction, operand: Value) {
        self.code.push(instruction as Value);
        self.code.push(operand);
    }
    #[inline(always)]
    fn add_string_literal(&mut self, literal: &str) -> Value {
        let address = self.data_segment.len();
        self.data_segment.extend_from_slice(literal.as_bytes());
        self.data_segment.push(0); // Null-terminate
        if address > i32::MAX as usize {
            panic!("Data segment address exceeds i32::MAX!");
        }
        address as Value
    }

    /// Reserves space for an instruction and operand, returning the index of the operand.
    #[inline(always)]
    fn reserve_instruction_operand(&mut self) -> usize {
        self.code.push(0); // Placeholder for instruction
        self.code.push(0); // Placeholder for operand
        self.code.len() - 1 // Return index of the operand
    }

    /// Patches the instruction and operand at the given operand index.
    #[inline(always)]
    fn patch_instruction_operand(&mut self, operand_index: usize, instruction: Instruction, operand: Value) {
        if operand_index == 0 || operand_index >= self.code.len() {
            panic!("Invalid patch location for instruction/operand: {}", operand_index);
        }
        self.code[operand_index - 1] = instruction as Value;
        self.code[operand_index] = operand;
    }

    // --- Parsing Rules ---

    /// Parses the entire program.
    pub fn parse_program(mut self) -> ParseResult<(Vec<Value>, Vec<u8>, Value)> {
        const PREAMBLE_SIZE: usize = 3;
        self.parse_global_declaration()?; // Expecting just main for now

        // Check for EOF after main
        match self.next_token() {
             Ok(TokenInfo { token: Token::Eof, .. }) => { /* Success */ }
             // Allow EndOfInput error here as success (means lexer exhausted after '}')
             Err(ParseError::EndOfInput) => { /* Success */ }
             // Any other token is an error
             Ok(other) => return Err(ParseError::UnexpectedToken {
                 expected: "end of file".to_string(), found: other.token, line: other.line, column: other.column
             }),
             // Any other error (e.g., lexer) is propagated
             Err(e) => return Err(e),
        }

        // Assemble final code
        let actual_main_address: Value = PREAMBLE_SIZE as Value;
        let preamble = vec![Instruction::Call as Value, actual_main_address, Instruction::Exit as Value];
        let final_code = [preamble, self.code].concat();
        Ok((final_code, self.data_segment, actual_main_address))
    }

    /// Parses a function definition (like 'int main() {...}').
    fn parse_global_declaration(&mut self) -> ParseResult<()> {
        self.expect_token(Token::Int, "type (int)")?;
        let (name, name_info) = self.expect_identifier("function name")?;
        if name != "main" { return Err(ParseError::Other(format!("Expected 'main', found '{}'", name))); }
        self.expect_token(Token::LParen, "'('")?;
        self.expect_token(Token::RParen, "')'")?;
        self.expect_token(Token::LBrace, "'{'")?;

        let main_relative_start_addr = self.code.len() as i64;
        // Use name_info for error reporting if add fails
        // FIX: Handle unused variable 'e' in error mapping
        let _ = self.symbol_table.add( name.clone(), Token::Ident(name.clone()), SymbolClass::Fun, DataType::Int, main_relative_start_addr )
            .map_err(|_e| { // Prefix e with underscore
                // Check error string content (a bit fragile, but works for now)
                if _e.contains("Redeclaration") {
                    ParseError::Redeclaration(name, name_info.line, name_info.column)
                } else {
                     ParseError::Other(format!("Failed to add main: {}", _e))
                }
             })?;

        self.symbol_table.enter_scope();
        self.current_func_locals_size = 0;
        self.ent_patch_location = Some(self.reserve_instruction_operand());
        self.patch_instruction_operand(self.ent_patch_location.unwrap(), Instruction::Ent, 0);

        // --- Parse function body statements ---
        loop {
            // Peek once at the start of the loop iteration
            let peek_result = self.peek_token();

            match peek_result {
                Ok(Some(Token::RBrace)) => {
                    // Found closing brace, consume it and exit loop
                    self.next_token()?;
                    break;
                }
                Ok(Some(_)) => {
                    // Found some other token, parse it as a statement
                    self.parse_statement()?;
                }
                Ok(None) => {
                    // Peek returned None, meaning EOF was reached *prematurely* inside the function body
                    let (line, col) = self.current_pos(); // Position of last valid token
                    return Err(ParseError::UnexpectedToken { expected: "'}' or statement".to_string(), found: Token::Eof, line, column: col });
                }
                Err(e @ ParseError::LexerError(_)) => {
                    // Peek failed due to lexer error - propagate directly
                    return Err(e);
                }
                 Err(e) => {
                     // Peek failed due to another error (e.g., EndOfInput from the underlying next())
                     // Propagate the original error `e` as it might be more informative
                     // than creating a generic UnexpectedToken.
                     return Err(e); // <<< FIX: Return original error >>>
                 }
            }
        } // --- End of statement loop ---


        // Patch ENT size
        if let Some(patch_loc) = self.ent_patch_location {
            if self.current_func_locals_size > i32::MAX as usize { return Err(ParseError::Other("Locals size overflow".to_string())); }
            self.code[patch_loc] = self.current_func_locals_size as Value;
        } else { return Err(ParseError::Other("Internal: ENT patch loc missing".to_string())); }

        // Add implicit LEV if needed
        let needs_lev = self.code.is_empty() || // Empty function needs LEV
                        self.code.last().map_or(true, |&v| Instruction::try_from(v) != Ok(Instruction::Lev));
        if needs_lev { self.emit(Instruction::Lev); }

        self.symbol_table.leave_scope();
        Ok(())
    }

     /// Parses a statement.
     fn parse_statement(&mut self) -> ParseResult<()> {
         // Peek first to decide statement type
         match self.peek_token()? {
             Some(Token::Int) | Some(Token::Char) => self.parse_declaration()?,
             Some(Token::Return) => {
                 self.next_token()?; // return
                 self.parse_expression()?; // value -> AX
                 self.expect_token(Token::Semicolon, "';'")?;
                 self.emit(Instruction::Lev); // Leave
             }
             Some(Token::LBrace) => { let ti = self.next_token()?; return Err(ParseError::Other(format!("Blocks not impl at {}:{}", ti.line, ti.column))); },
             Some(Token::If) | Some(Token::While) => { let ti = self.next_token()?; return Err(ParseError::Other(format!("Control flow not impl at {}:{}", ti.line, ti.column))); },
             Some(Token::Semicolon) => { self.next_token()?; } // Empty
             // Handle EOF explicitly if peeked (should be caught by loop above, but defensive)
             Some(Token::Eof) | None => return Err(ParseError::UnexpectedToken { expected: "statement".to_string(), found: Token::Eof, line: self.current_pos().0, column: self.current_pos().1 }),
             _ => { // Expression statement
                 self.parse_expression()?;
                 self.expect_token(Token::Semicolon, "';'")?;
             }
         }
         Ok(())
     }

      /// Parses a local variable declaration.
      fn parse_declaration(&mut self) -> ParseResult<()> {
        let type_token = self.next_token()?;
        let data_type = match type_token.token {
            Token::Int => DataType::Int, Token::Char => DataType::Char,
            _ => return Err(ParseError::UnexpectedToken { expected: "type".to_string(), found: type_token.token, line: type_token.line, column: type_token.column }),
        };
        let (name, name_info) = self.expect_identifier("variable name")?;

        let size_of_local = 1;
        self.current_func_locals_size += size_of_local;
        let local_offset = -(self.current_func_locals_size as i64);

        // Use name_info for error reporting if add fails
        // FIX: Handle unused variable 'e'
        let _ = self.symbol_table.add( name.clone(), Token::Ident(name.clone()), SymbolClass::Loc, data_type, local_offset )
            .map_err(|_e| { // Prefix with underscore
                if _e.contains("Redeclaration") {
                    ParseError::Redeclaration(name, name_info.line, name_info.column)
                } else {
                     ParseError::Other(format!("Failed to add local var: {}", _e))
                }
             })?;

        if self.peek_token()? == Some(&Token::Assign) {
            self.next_token()?; // =
            self.emit_with_operand(Instruction::Lea, local_offset as Value);
            self.emit(Instruction::Push);
            self.parse_expression()?; // initializer -> AX
            self.emit(Instruction::Si);
        }
        self.expect_token(Token::Semicolon, "';'")?;
        Ok(())
    }

    /// Parses an expression (entry point).
    fn parse_expression(&mut self) -> ParseResult<()> { self.parse_assignment_expression() }

    /// Parses assignment expressions (`=`), right-associative.
    fn parse_assignment_expression(&mut self) -> ParseResult<()> {
        let lhs_is_lvalue = self.parse_additive_expression_check_lvalue()?;
        if self.peek_token()? == Some(&Token::Assign) {
            self.next_token()?; // =
            if !lhs_is_lvalue {
                let (line, col) = self.current_pos();
                let desc = self.current_token_info.as_ref().map_or("expr".into(), |ti|format!("{:?}", ti.token));
                return Err(ParseError::NotAssignable(desc, line, col));
            }
            self.emit(Instruction::Push); // Push addr (already in AX from LEA in LHS)
            self.parse_assignment_expression()?; // Parse RHS -> AX
            self.emit(Instruction::Si); // Store AX -> [Pop()]
        }
        Ok(())
    }


    /// Parses additive expressions (`+`, `-`), left-associative.
    fn parse_additive_expression_check_lvalue(&mut self) -> ParseResult<bool> {
        let mut could_be_lvalue = self.parse_primary()?; // Parse first term
        loop {
            match self.peek_token()? {
                Some(Token::Add) | Some(Token::Sub) => {
                    could_be_lvalue = false; // Result of +/- is not lvalue
                    let op = self.next_token()?.token;
                    self.emit(Instruction::Push); // Push LHS (in AX)
                    let _ = self.parse_primary()?; // Parse RHS -> AX
                    match op {
                        Token::Add => self.emit(Instruction::Add),
                        Token::Sub => self.emit(Instruction::Sub),
                        _ => unreachable!(),
                    }
                }
                _ => break, // No more + or -
            }
        }
        Ok(could_be_lvalue)
    }


    /// Parses primary expressions: literals, identifiers (vars/calls), parentheses.
    fn parse_primary(&mut self) -> ParseResult<bool> {
        let token_info = self.next_token()?;
        let mut emitted_lea = false;
        match token_info.token {
            Token::Number(val) => {
                 // Check range before cast
                 if val < i32::MIN as i64 || val > i32::MAX as i64 {
                    return Err(ParseError::Other(format!("Integer {} out of range at {}:{}", val, token_info.line, token_info.column)));
                 }
                 self.emit_with_operand(Instruction::Imm, val as Value);
            }
            Token::StringLiteral(s) => { let addr = self.add_string_literal(&s); self.emit_with_operand(Instruction::Imm, addr); }
            Token::Ident(name) => {
                 if self.peek_token()? == Some(&Token::LParen) {
                     self.next_token()?; // Consume '('
                     self.parse_function_call(name, token_info.line, token_info.column)?; // Handles call logic
                 } else { // Variable access
                     let sym = self.symbol_table.find(&name).ok_or_else(|| ParseError::UndefinedSymbol(name.clone(), token_info.line, token_info.column))?;
                     match sym.class {
                         SymbolClass::Loc | SymbolClass::Glo => {
                             self.emit_with_operand(Instruction::Lea, sym.value as Value); // Addr -> AX
                             emitted_lea = true;
                             self.emit(Instruction::Li); // Value -> AX
                         }
                         _ => return Err(ParseError::NotAVariable(name.clone(), token_info.line, token_info.column)),
                     }
                 }
            }
            Token::LParen => { self.parse_expression()?; self.expect_token(Token::RParen, "')'")?; }
            // Allow EOF here only if it's the very first token read in primary
            Token::Eof => return Err(ParseError::UnexpectedToken { expected: "primary expression".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
           _ => return Err(ParseError::UnexpectedToken { expected: "primary expr".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
        }
        Ok(emitted_lea) // Return true if we ended by calculating an address (LEA)
    }

     // --- Function Call Parsing with Correct Argument Order ---
     /// Parses a function call like `name(...)`. Assumes identifier and '(' consumed.
     /// Generates code to evaluate arguments L->R, PUSH them R->L, then CALL.
     fn parse_function_call(&mut self, name: String, line: usize, column: usize) -> ParseResult<()> {
        // Find function in symbol table
        let func_entry = self.symbol_table.find(&name).cloned()
            .ok_or_else(|| ParseError::UndefinedSymbol(name.clone(), line, column))?;

        let func_addr_i64 = match func_entry.class {
            SymbolClass::Fun | SymbolClass::Sys => func_entry.value,
            _ => return Err(ParseError::NotAFunction(name.clone(), line, column)),
        };

        let mut arg_codes: Vec<Vec<Value>> = Vec::new();
        let mut temp_code_buffer: Vec<Value> = Vec::new();
        let mut arg_count = 0; // Initialize arg_count here

        // 1. Parse arguments L->R, generating code for each into temporary buffers
        if self.peek_token()? != Some(&Token::RParen) {
            loop {
                // Ensure the main code buffer is empty before swapping
                assert!(temp_code_buffer.is_empty());
                mem::swap(&mut self.code, &mut temp_code_buffer);

                // Parse argument expression -> temp_code_buffer
                self.parse_expression()?;

                // Swap back - temp_code_buffer holds arg code, self.code restored
                mem::swap(&mut self.code, &mut temp_code_buffer);

                arg_codes.push(temp_code_buffer.clone());
                temp_code_buffer.clear();
                arg_count += 1; // Increment arg_count after successfully parsing an argument

                // Decide next step: Consume ',' or break on ')' or error
                // --- FIX: Consume the token *then* match ---
                let separator_token_info = self.next_token()?;
                match separator_token_info.token {
                    Token::Comma => { /* Correct, continue loop */ }
                    Token::RParen => { break; } // End of args, exit loop
                    other => { // Unexpected token after argument
                         return Err(ParseError::UnexpectedToken{
                            expected: "',' or ')'".into(),
                            found: other, // The token we actually consumed
                            line: separator_token_info.line,
                            column: separator_token_info.column
                         });
                    }
                }
            } // End arg parsing loop
        } else {
            // No arguments, just consume the closing parenthesis
            self.expect_token(Token::RParen, "')' for function with no arguments")?;
        }
        // arg_count is now correctly set based on the loop execution

        // 2. Emit argument code and PUSH instructions in REVERSE order
        for arg_code in arg_codes.iter().rev() {
            // Append the code that calculates the argument value
            self.code.extend_from_slice(arg_code);
            // Push the result (which the arg_code left in AX)
            self.emit(Instruction::Push);
        }

        // 3. Emit CALL instruction
        if func_addr_i64 != PRINTF_SYSCALL_ADDR && (func_addr_i64 < i32::MIN as i64 || func_addr_i64 > i32::MAX as i64) {
            return Err(ParseError::Other(format!("Function address {} for '{}' out of i32 range", func_addr_i64, name)));
        }
        let func_addr_val = func_addr_i64 as Value;
        self.emit_with_operand(Instruction::Call, func_addr_val);

        // 4. Emit ADJ to clean up stack (C caller cleanup)
        if arg_count > 0 {
            if arg_count > i32::MAX as usize { return Err(ParseError::Other(format!("Argument count {} exceeds i32::MAX", arg_count))); }
            self.emit_with_operand(Instruction::Adj, arg_count as Value);
        }

        // Result of the function call is now in AX
        Ok(())
    } // End parse_function_call

} // impl Parser