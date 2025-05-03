//! Parser for a C4 subset, aiming for self-hosting capabilities.
//! Directly emits VM instructions.

// NOTE: Ensure you have added `#[derive(Clone)]` to your Lexer struct definition in lexer.rs!
use crate::lexer::{Lexer, Token, TokenInfo, LexerError};
use crate::symbol::{SymbolTable, SymbolClass, DataType};
use crate::vm::{Instruction, Value}; // Assuming TryFrom<Value> for Instruction is defined HERE
use std::iter::Peekable;
use std::mem; // Needed for mem::swap

// --- Operator Precedence Levels (Higher value = higher precedence) ---
// Matches C4's implicit levels used in expr()
const PREC_BOTTOM: i32 = 0; // Sentinel
const PREC_ASSIGN: i32 = 1; // =
const PREC_COND:   i32 = 2; // ?:
const PREC_LOR:    i32 = 3; // ||
const PREC_LAN:    i32 = 4; // &&
const PREC_OR:     i32 = 5; // |
const PREC_XOR:    i32 = 6; // ^
const PREC_AND:    i32 = 7; // &
const PREC_EQ:     i32 = 8; // == !=
const PREC_REL:    i32 = 9; // < <= > >=
const PREC_SHIFT:  i32 = 10; // << >>
const PREC_ADD:    i32 = 11; // + -
const PREC_MUL:    i32 = 12; // * / %
const PREC_UNARY:  i32 = 13; // ++ -- (prefix) * & ! ~ + - (unary) sizeof cast
const PREC_POSTFIX:i32 = 14; // () [] ++ -- (postfix)

// --- Parser Error (Keep existing definition or enhance as needed) ---
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
    NotAnLValue(String, usize, usize), // Trying to operate on non-lvalue (e.g., ++(a+b))
    NotAssignable(String, usize, usize), // Trying to assign to non-lvalue
    InvalidCastTarget(usize, usize),
    InvalidSizeofTarget(usize, usize),
    InvalidDereference(usize, usize),
    InvalidAddressOf(usize, usize),
    TypeMismatch(String, usize, usize), // E.g., pointer arithmetic issues
    EndOfInput, // Reached end of input unexpectedly
    Other(String), // Generic error
    Redeclaration(String, usize, usize), // For variables/functions/enums in the same scope
}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError(err)
    }
}

// Add a basic Display impl if you don't have one
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::LexerError(e) => write!(f, "Lexer error: {}", e),
            ParseError::UnexpectedToken { expected, found, line, column } => {
                let found_str = if *found == Token::Eof { "end of file".to_string() } else { format!("{:?}", found) };
                write!(f, "Parse error at {}:{}: Expected {}, found {}", line, column, expected, found_str)
            }
            ParseError::UndefinedSymbol(name, line, col) => write!(f, "Parse error at {}:{}: Undefined symbol '{}'", line, col, name),
            ParseError::NotAFunction(name, line, col) => write!(f, "Parse error at {}:{}: '{}' is not a function", line, col, name),
            ParseError::NotAVariable(name, line, col) => write!(f, "Parse error at {}:{}: '{}' is not a variable or parameter", line, col, name),
            ParseError::NotAnLValue(desc, line, col) => write!(f, "Parse error at {}:{}: Cannot take address of or modify non-lvalue '{}'", line, col, desc),
            ParseError::NotAssignable(desc, line, col) => write!(f, "Parse error at {}:{}: Cannot assign to non-lvalue '{}'", line, col, desc),
            ParseError::InvalidCastTarget(line, col) => write!(f, "Parse error at {}:{}: Invalid target for type cast", line, col),
            ParseError::InvalidSizeofTarget(line, col) => write!(f, "Parse error at {}:{}: Invalid target for sizeof", line, col),
            ParseError::InvalidDereference(line, col) => write!(f, "Parse error at {}:{}: Cannot dereference non-pointer type", line, col),
            ParseError::InvalidAddressOf(line, col) => write!(f, "Parse error at {}:{}: Cannot take address of this expression", line, col),
            ParseError::TypeMismatch(msg, line, col) => write!(f, "Type error at {}:{}: {}", line, col, msg),
            ParseError::EndOfInput => write!(f, "Parse error: Unexpected end of input"),
            ParseError::Other(msg) => write!(f, "Parse error: {}", msg),
            ParseError::Redeclaration(name, line, col) => write!(f, "Parse error at {}:{}: Redeclaration of symbol '{}'", line, col, name),
        }
    }
}

impl std::error::Error for ParseError {}


// --- Parser Implementation ---

type ParseResult<T> = Result<T, ParseError>;

// Structure to hold information about the expression being parsed
#[derive(Clone, Debug)]
struct ExprInfo {
    data_type: DataType,
    // Is the value currently in AX an address (LValue) that can be stored into?
    // false if it's a temporary result (RValue).
    is_lvalue: bool,
    // If is_lvalue is true, this is the instruction used to load the value (LI/LC)
    // Needed for post-increment/decrement and deferred loading.
    lvalue_load_instr: Option<Instruction>,
}

pub struct Parser<'a> {
    // IMPORTANT: Lexer struct MUST derive Clone for peek_is_function_definition
    tokens: Peekable<Lexer<'a>>,
    symbol_table: SymbolTable,
    code: Vec<Value>,      // Main code buffer
    data_segment: Vec<u8>, // Global/static data (including string literals)
    current_token_info: Option<TokenInfo>, // Stores the *last consumed* token info

    // Function-specific state:
    // Tracks the current offset for the *next* local variable relative to BP (starts negative).
    current_bp_offset: i64,
    // Index in `code` where the ENT instruction's operand (local frame size) needs patching.
    ent_patch_location: Option<usize>,
}

// Define constant for built-ins if not already defined globally
// Use a distinct value if needed, -1 is common but ensure it doesn't clash
const PRINTF_SYSCALL_ADDR: i64 = -1;
// Add others as needed:
// const MALLOC_SYSCALL_ADDR: i64 = -2;
// const FREE_SYSCALL_ADDR:   i64 = -3;
// ...

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut symbol_table = SymbolTable::new();

        // --- Add built-in keywords and functions ---
        // Keywords (simplifies checking later)
        symbol_table.add_reserved("char", Token::Char).unwrap();
        symbol_table.add_reserved("else", Token::Else).unwrap();
        symbol_table.add_reserved("enum", Token::Enum).unwrap();
        symbol_table.add_reserved("if", Token::If).unwrap();
        symbol_table.add_reserved("int", Token::Int).unwrap();
        symbol_table.add_reserved("return", Token::Return).unwrap();
        symbol_table.add_reserved("sizeof", Token::Sizeof).unwrap();
        symbol_table.add_reserved("while", Token::While).unwrap();
        symbol_table.add_reserved("void", Token::Void).unwrap(); // Add void if it's a token

        // Built-in functions (using Sys class)
        // Need to define their VM opcodes or syscall numbers in vm.rs or globally
        symbol_table.add(
            "printf".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, PRINTF_SYSCALL_ADDR
        ).expect("Failed to add built-in printf");
        // Add others like open, read, close, malloc, free, memset, memcmp, exit here
        // Example:
        // symbol_table.add("malloc".to_string(), Token::Sys, SymbolClass::Sys, DataType::Ptr(Box::new(DataType::Void)), MALLOC_SYSCALL_ADDR).unwrap();
        // symbol_table.add("exit".to_string(), Token::Sys, SymbolClass::Sys, DataType::Void, EXIT_SYSCALL_ADDR).unwrap();


        Parser {
            tokens: lexer.peekable(),
            symbol_table,
            code: Vec::new(),
            data_segment: Vec::new(),
            current_token_info: None,
            current_bp_offset: 0, // Reset for each function
            ent_patch_location: None,
        }
    }

    // --- Token Helpers (Keep existing or refine) ---
    #[inline]
    fn next_token(&mut self) -> ParseResult<TokenInfo> {
         match self.tokens.next() {
            Some(Ok(token_info)) => {
                self.current_token_info = Some(token_info.clone());
                Ok(token_info)
            }
            Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err)),
            None => Err(ParseError::EndOfInput),
        }
    }
    #[inline]
    fn peek_token(&mut self) -> ParseResult<Option<&Token>> {
         match self.tokens.peek() {
             Some(Ok(token_info)) => Ok(Some(&token_info.token)),
             Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err.clone())), // Clone error if needed
             None => Ok(None),
         }
     }
    #[inline]
    fn current_pos(&self) -> (usize, usize) {
        self.current_token_info.as_ref().map_or((0, 0), |ti| (ti.line, ti.column))
    }
    #[inline]
     fn expect_token(&mut self, expected_token: Token, expected_desc: &str) -> ParseResult<TokenInfo> {
         let token_info = self.next_token()?;
         if token_info.token == expected_token {
             Ok(token_info)
         } else {
             Err(ParseError::UnexpectedToken {
                 expected: expected_desc.to_string(),
                 found: token_info.token,
                 line: token_info.line,
                 column: token_info.column,
             })
         }
     }
     #[inline]
     fn expect_identifier(&mut self, expected_desc: &str) -> ParseResult<(String, TokenInfo)> {
        let token_info = self.next_token()?;
        if let Token::Ident(name) = token_info.token.clone() {
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
    // Helper to consume token only if it matches
    #[inline]
    fn consume_optional(&mut self, token_to_match: Token) -> ParseResult<bool> {
        if self.peek_token()? == Some(&token_to_match) {
            self.next_token()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }


    // --- Code Emission Helpers (Keep existing or refine) ---
    #[inline] fn emit(&mut self, instruction: Instruction) { self.code.push(instruction as Value); }
    #[inline] fn emit_with_operand(&mut self, instruction: Instruction, operand: Value) { self.emit(instruction); self.code.push(operand); }
    #[inline] fn current_code_addr(&self) -> usize { self.code.len() }
    #[inline]
    fn add_string_literal(&mut self, literal: &str) -> Value { // Ensure return type is Value
        let address = self.data_segment.len();
        self.data_segment.extend_from_slice(literal.as_bytes());
        self.data_segment.push(0); // Null-terminate
        if address > i32::MAX as usize {
            // Consider using u64 for addresses if needed, but C4 likely assumes 32-bit
            panic!("Data segment address exceeds i32::MAX!");
        }
        address as Value
    }

    // Patching for jumps
    #[inline] fn reserve_jump_operand(&mut self, jump_instruction: Instruction) -> usize {
        self.emit(jump_instruction);
        self.code.push(0); // Placeholder for target address
        self.current_code_addr() - 1 // Return index of the operand
    }
    #[inline] fn patch_jump_target(&mut self, operand_index: usize, target_addr: usize) {
        if operand_index >= self.code.len() { panic!("Invalid patch location"); }
         if target_addr > i32::MAX as usize { panic!("Jump target address overflow"); }
        self.code[operand_index] = target_addr as Value;
    }
    // Helpers for emitting load/store based on type
    fn emit_load(&mut self, expr_type: &DataType) -> Result<(), ParseError> {
        self.emit(match expr_type {
            DataType::Char => Instruction::Lc,
            DataType::Int | DataType::Ptr(_) => Instruction::Li, // Ints and Pointers load full word
            DataType::Void => return Err(ParseError::Other("Cannot load value of type void".into())),
        });
        Ok(())
    }
    fn emit_store(&mut self, expr_type: &DataType) -> Result<(), ParseError> {
        self.emit(match expr_type {
            DataType::Char => Instruction::Sc,
            DataType::Int | DataType::Ptr(_) => Instruction::Si,
            DataType::Void => return Err(ParseError::Other("Cannot store value of type void".into())),
        });
        Ok(())
    }


    // --- Parsing Rules ---

    /// Parses the entire program (sequence of global declarations).
    pub fn parse_program(mut self) -> ParseResult<(Vec<Value>, Vec<u8>, Value)> {
        // Add preamble: CALL main, EXIT
        // We don't know main's address yet, so patch later.
        self.emit(Instruction::Call);
        let main_call_patch_loc = self.current_code_addr();
        self.code.push(0); // Placeholder for main's address
        self.emit(Instruction::Exit);
        let _preamble_size = self.current_code_addr(); // Should be 3 (Marked unused)

        // Parse global declarations (vars or functions)
        while self.peek_token()? != Some(&Token::Eof) {
             // We need to peek deeper to see if it's a function or variable
            let is_func = self.peek_is_function_definition()?;
            self.parse_global_declaration(is_func)?;
        }

        // Check if main was defined and patch the call
        let main_val = if let Some(main_sym) = self.symbol_table.find("main") {
            if main_sym.class != SymbolClass::Fun {
                return Err(ParseError::Other("'main' is not defined as a function".into()));
            }
             // Ensure address fits in Value
            if main_sym.value < i32::MIN as i64 || main_sym.value > i32::MAX as i64 {
                 return Err(ParseError::Other(format!("'main' function address {} out of range", main_sym.value)));
            }
            let main_val = main_sym.value as Value;
            self.code[main_call_patch_loc] = main_val;
            main_val // Return the value directly
        } else {
            return Err(ParseError::Other("'main' function not defined".into()));
        };

        Ok((self.code, self.data_segment, main_val)) // Return final code, data, and main addr
    }

     /// Peeks ahead to guess if the next global declaration is a function definition.
     /// Looks for `type ident (` pattern. Very simplified.
     /// Requires Lexer to be Clone.
    fn peek_is_function_definition(&mut self) -> ParseResult<bool> {
        // This is tricky without full lookahead or backtracking.
        // A simple heuristic: if we see `type identifier (`, assume function.
        // Let's clone the iterator state for peeking.
        // NOTE: This requires `Lexer` to derive `Clone`. Ensure it does!
        let mut temp_lexer = self.tokens.clone(); // Clone the Peekable<Lexer>
        let mut lookahead_count = 0;
        let max_lookahead = 5; // Limit lookahead depth

        // Consume type (int/char/enum/void)
        match temp_lexer.next() {
            Some(Ok(TokenInfo { token: Token::Int | Token::Char | Token::Enum | Token::Void, .. })) => lookahead_count += 1,
            _ => return Ok(false), // Not starting with a known type
        }
        if lookahead_count >= max_lookahead { return Ok(false); } // Avoid deep lookahead

        // Consume optional pointers (*)
        while let Some(Ok(TokenInfo { token: Token::Asterisk, .. })) = temp_lexer.peek() {
             temp_lexer.next(); // Consume '*'
             lookahead_count += 1;
              if lookahead_count >= max_lookahead { return Ok(false); }
        }

        // Consume identifier
         match temp_lexer.next() {
             Some(Ok(TokenInfo { token: Token::Ident(_), .. })) => lookahead_count += 1,
             _ => return Ok(false), // Not an identifier after type/pointers
         }
          if lookahead_count >= max_lookahead { return Ok(false); }

        // Check for '('
         match temp_lexer.peek() {
             Some(Ok(TokenInfo { token: Token::LParen, .. })) => Ok(true), // Found `type *... ident (` pattern
             _ => Ok(false),
         }
    }


    /// Parses one global declaration (variable, enum definition, or function).
    fn parse_global_declaration(&mut self, is_function_hint: bool) -> ParseResult<()> {
        let base_type: DataType;
        let _start_line = self.current_pos().0; // Approx line - FIX: Mark unused

        // --- Parse Base Type or Enum ---
        if self.peek_token()? == Some(&Token::Enum) {
            self.parse_enum_declaration()?;
            // Enum declaration doesn't declare a variable/function directly after
            return Ok(()); // Handled inside parse_enum_declaration
        } else if self.peek_token()? == Some(&Token::Int) {
            self.next_token()?; base_type = DataType::Int;
        } else if self.peek_token()? == Some(&Token::Char) {
            self.next_token()?; base_type = DataType::Char;
        } else if self.peek_token()? == Some(&Token::Void) {
            self.next_token()?; base_type = DataType::Void;
        } else {
             let token_info = self.next_token()?;
             return Err(ParseError::UnexpectedToken { expected: "type (int, char, void, enum)".into(), found: token_info.token, line: token_info.line, column: token_info.column });
        }

        // --- Parse potential variable/function list ---
        loop {
            let current_type = self.parse_pointers(base_type.clone())?;
            let (name, name_info) = self.expect_identifier("variable or function name")?;

            // --- Check if Function or Variable ---
             if self.peek_token()? == Some(&Token::LParen) {
                 if !is_function_hint && self.symbol_table.get_current_scope() != 0 {
                     // Warn or error if we expected a variable based on hint but found '('
                     // For now, proceed assuming it's a function
                 }
                 if base_type == DataType::Void && current_type == DataType::Void {
                     // Allow void main(), void func(), etc.
                 } else if current_type == DataType::Void {
                     return Err(ParseError::Other(format!("Function '{}' cannot return void pointer at {}:{}", name, name_info.line, name_info.column)));
                 }
                 self.parse_function_definition(name, name_info, current_type)?;
                 // Function definition ends the declaration line
                 return Ok(()); // Expect ; or { handled inside parse_function_definition
             } else {
                  if is_function_hint {
                     // Warn or error if we expected a function based on hint
                     // For now, proceed assuming it's a variable
                 }
                 if current_type == DataType::Void {
                     return Err(ParseError::Other(format!("Cannot declare variable '{}' of type void at {}:{}", name, name_info.line, name_info.column)));
                 }
                 // Global Variable Declaration
                 let _sym = self.symbol_table.add(name.clone(), Token::Ident(name.clone()), SymbolClass::Glo, current_type.clone(), self.data_segment.len() as i64)
                    .map_err(|e| Parser::map_add_error(e, &name, name_info.line, name_info.column))?; // Use static map_add_error

                 // Allocate space in data segment (simple: assume size of Value for int/ptr, byte for char)
                 let size = current_type.size(); // Use size() method
                 for _ in 0..size { self.data_segment.push(0); } // Initialize global to 0

                 // Handle potential initializers if supported for globals (C4 doesn't really)
                 if self.consume_optional(Token::Assign)? {
                     // C4 doesn't support complex global initializers that require code execution.
                     // Could potentially handle constant literals here.
                     return Err(ParseError::Other(format!("Global initializers not supported at {}:{}", name_info.line, name_info.column)));
                 }
             }

            // Check for end of declaration list (;) or another declaration (,)
            if self.consume_optional(Token::Comma)? {
                continue; // Parse next variable in the list
            } else {
                self.expect_token(Token::Semicolon, "';' or ',' after global declaration")?;
                return Ok(()); // End of this global declaration line
            }
        } // End loop for vars on one line
    }

    /// Parses '*' pointers after a base type.
    fn parse_pointers(&mut self, mut base_type: DataType) -> ParseResult<DataType> {
        while self.consume_optional(Token::Asterisk)? {
            base_type = DataType::Ptr(Box::new(base_type));
        }
        Ok(base_type)
    }

     /// Parses an enum declaration `enum [tag] { NAME1, NAME2 = val, ... };`
     /// Adds enum members to the current scope.
    fn parse_enum_declaration(&mut self) -> ParseResult<()> {
        self.expect_token(Token::Enum, "enum keyword")?;

        // Optional enum tag (C4 doesn't use tags much, we can ignore or store if needed)
        let mut _enum_tag = None; // Marked unused
        if let Some(Token::Ident(_name)) = self.peek_token()?.cloned() { // Clone to avoid borrow issue with expect_identifier
            let (tag_name, _) = self.expect_identifier("enum tag")?;
            _enum_tag = Some(tag_name); // Marked unused
            // Optionally add tag to symbol table with SymbolClass::Enum if needed later
        }

        self.expect_token(Token::LBrace, "'{' for enum definition")?;

        let mut current_enum_value: i64 = 0;
        loop {
            if self.peek_token()? == Some(&Token::RBrace) { break; }

            let (name, name_info) = self.expect_identifier("enum member name")?;

            // Optional assignment
            if self.consume_optional(Token::Assign)? {
                 // Allow optional unary minus before number literal
                let negative = self.consume_optional(Token::Sub)?;
                let val_token = self.expect_token(Token::Number(0), "integer value for enum member")?; // Expect Num, value irrelevant here
                if let Token::Number(mut num_val) = val_token.token {
                     if negative { num_val = -num_val; }
                     current_enum_value = num_val;
                } else { unreachable!(); } // Should be caught by expect_token if lexer is correct
            }

            // Add enum member to symbol table
            self.symbol_table.add(name.clone(), Token::Ident(name.clone()), SymbolClass::Num, DataType::Int, current_enum_value)
                 .map_err(|e| Parser::map_add_error(e, &name, name_info.line, name_info.column))?; // Use static map_add_error

            current_enum_value += 1; // Auto-increment for next member

            if self.consume_optional(Token::Comma)? {
                // Check for trailing comma before brace
                if self.peek_token()? == Some(&Token::RBrace) { break; }
                continue;
            } else if self.peek_token()? == Some(&Token::RBrace) {
                break;
            } else {
                 let ti = self.next_token()?;
                 return Err(ParseError::UnexpectedToken { expected: "',' or '}' in enum definition".into(), found: ti.token, line: ti.line, column: ti.column });
            }
        }
        self.expect_token(Token::RBrace, "'}' after enum definition")?;
        // Enum definition itself doesn't end with ';', but the *declaration* using it might
        // C4 allows `enum {...} var;` but we handle that in parse_global_declaration
        // If it's just `enum {...};`, consume the semicolon.
        self.consume_optional(Token::Semicolon)?; // Optional semicolon after enum def

        Ok(())
    }


    /// Parses a function definition `type *... name (...) { ... }`
    fn parse_function_definition(&mut self, name: String, name_info: TokenInfo, return_type: DataType) -> ParseResult<()> {
         let func_start_addr = self.current_code_addr() as i64;

         // Add function symbol to GLOBAL scope before parsing body/params
         self.symbol_table.add(name.clone(), Token::Ident(name.clone()), SymbolClass::Fun, return_type.clone(), func_start_addr)
             .map_err(|e| Parser::map_add_error(e, &name, name_info.line, name_info.column))?; // Use static map_add_error

         self.expect_token(Token::LParen, "'(' for function definition")?;

         // Enter function scope
         self.symbol_table.enter_scope();
         let mut param_count = 0; // Track param count for ADJ later
         let mut param_symbols: Vec<(String, TokenInfo, DataType)> = Vec::new(); // Store params temporarily

         // Parse parameters
         if self.peek_token()? != Some(&Token::RParen) {
             loop {
                 // Parse param type
                 let base_type = match self.peek_token()? {
                    Some(Token::Int) => { self.next_token()?; DataType::Int },
                    Some(Token::Char) => { self.next_token()?; DataType::Char },
                    Some(Token::Void) => { self.next_token()?; DataType::Void }, // Allow void param? C doesn't really.
                    _ => { let ti = self.next_token()?; return Err(ParseError::UnexpectedToken { expected:"parameter type".into(), found:ti.token, line:ti.line, column:ti.column }); }
                 };
                 let param_type = self.parse_pointers(base_type)?;
                 // Check for void param - only allowed if it's the *only* param and name omitted?
                 if param_type == DataType::Void && param_count > 0 {
                     let (l,c) = self.current_pos();
                     return Err(ParseError::Other(format!("'void' must be the only parameter at {}:{}", l, c)));
                 }

                 let (param_name, param_name_info) = self.expect_identifier("parameter name")?;

                 param_symbols.push((param_name, param_name_info, param_type));
                 param_count += 1;

                 if !self.consume_optional(Token::Comma)? { break; }
             }
         }
         self.expect_token(Token::RParen, "')' after function parameters")?;
         self.expect_token(Token::LBrace, "'{' to start function body")?;

         // Now add parameters to symbol table with correct offsets
         // In C4's model (and many C conventions), args are *above* BP.
         // If BP points to old BP, args start at BP+2 (BP+1 is return addr).
         // Offset is (param_index + 2) relative to BP (in words/slots).
         for (i, (p_name, p_info, p_type)) in param_symbols.iter().enumerate() {
             let param_offset = (i + 2) as i64; // Offset in words relative to BP
             self.symbol_table.add(p_name.clone(), Token::Ident(p_name.clone()), SymbolClass::Loc, p_type.clone(), param_offset)
                  .map_err(|e| Parser::map_add_error(e, p_name, p_info.line, p_info.column))?; // Use static map_add_error
         }

        // --- Prepare for Locals ---
        // `current_bp_offset` tracks offset for *next* local relative to BP (starts at 0, becomes -1, -2...)
        self.current_bp_offset = 0; // Reset for this function
        let mut max_bp_offset_count = 0; // Track total size needed for locals (in words/slots)

        // Reserve space for ENT instruction (operand is number of local variable words)
        self.emit(Instruction::Ent);
        self.ent_patch_location = Some(self.current_code_addr());
        self.code.push(0); // Placeholder for size


        // --- Parse Local Declarations ---
        while matches!(self.peek_token()?, Some(Token::Int | Token::Char)) {
            max_bp_offset_count = self.parse_local_declaration(max_bp_offset_count)?;
        }

        // --- Parse Function Body Statements ---
        while self.peek_token()? != Some(&Token::RBrace) {
            if self.peek_token()? == Some(&Token::Eof) {
                let (l, c) = self.current_pos();
                 return Err(ParseError::UnexpectedToken { expected: "'}' to end function body".into(), found: Token::Eof, line: l, column: c });
            }
            self.parse_statement()?;
        }
        self.expect_token(Token::RBrace, "'}' to end function body")?;

        // --- Patch ENT and Emit LEV ---
        if let Some(patch_loc) = self.ent_patch_location {
             let locals_size_words = max_bp_offset_count;
             if locals_size_words > i32::MAX as usize { return Err(ParseError::Other("Locals size overflow".to_string())); }
             self.code[patch_loc] = locals_size_words as Value;
        } else {
             // Should not happen if ENT was emitted
             return Err(ParseError::Other("Internal error: Missing ENT patch location".to_string()));
        }

        // Add implicit LEV if last instruction wasn't already LEV (from a return)
        let mut needs_lev = true;
        if let Some(&last_instr_val) = self.code.last() {
            // Use the TryFrom defined in vm.rs (assuming it returns Result<_, VmError>)
            if let Ok(instr) = Instruction::try_from(last_instr_val) {
                if instr == Instruction::Lev {
                    needs_lev = false;
                }
            }
            // If try_from fails, it wasn't LEV anyway.
        }
        if needs_lev {
            self.emit(Instruction::Lev);
        }


        // Leave function scope
        self.symbol_table.leave_scope();
        Ok(())
    }


     /// Parses local variable declarations after function entry / block start.
     /// Returns the updated maximum local variable offset count (number of slots).
    fn parse_local_declaration(&mut self, mut max_offset_count: usize) -> ParseResult<usize> {
        let base_type = match self.peek_token()? {
            Some(Token::Int) => { self.next_token()?; DataType::Int },
            Some(Token::Char) => { self.next_token()?; DataType::Char },
            _=> unreachable!(), // Caller ensures Int or Char
        };

        loop { // Parse multiple variables separated by comma
            let var_type = self.parse_pointers(base_type.clone())?;
            let (name, name_info) = self.expect_identifier("local variable name")?;

            // Calculate offset for this local variable
            // C4 allocates space based on type size, but uses word offsets for LEA.
            // Let's stick to word offsets like C4 for simplicity.
            let size_in_words = var_type.size_in_words(); // Use size_in_words from symbol.rs
            self.current_bp_offset -= size_in_words; // Decrement by size (e.g., -1 for int/ptr/char)
            let current_offset = self.current_bp_offset;
             // Update max size needed (absolute value of most negative offset -> count)
             max_offset_count = max_offset_count.max((-current_offset) as usize);

             // Add local variable to symbol table
             self.symbol_table.add(name.clone(), Token::Ident(name.clone()), SymbolClass::Loc, var_type.clone(), current_offset)
                .map_err(|e| Parser::map_add_error(e, &name, name_info.line, name_info.column))?; // Use static map_add_error

             // Handle optional initializer
            if self.consume_optional(Token::Assign)? {
                 // Emit code to get address of local (LEA offset)
                self.emit_with_operand(Instruction::Lea, current_offset as Value); // Addr -> AX
                self.emit(Instruction::Push); // Push address onto stack
                // Parse initializer expression -> result in AX
                let expr_info = self.parse_expression_with_level(PREC_ASSIGN)?;
                // Load value if initializer was LValue
                let mut init_info = expr_info; // Rename to avoid confusion
                if init_info.is_lvalue {
                    if let Some(load_instr) = init_info.lvalue_load_instr {
                        self.emit(load_instr);
                        init_info.is_lvalue = false;
                    }
                }
                // TODO: Type check initializer against var_type?
                // Emit store instruction (pops address, stores AX)
                self.emit_store(&var_type)?;
            }

            if !self.consume_optional(Token::Comma)? { break; } // Exit loop if no comma
        }
        self.expect_token(Token::Semicolon, "';' after local declaration")?;
         Ok(max_offset_count) // Return updated max offset count
    }


    /// Parses a statement.
    fn parse_statement(&mut self) -> ParseResult<()> {
        match self.peek_token()? {
            Some(Token::If) => self.parse_if_statement(),
            Some(Token::While) => self.parse_while_statement(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::LBrace) => self.parse_block_statement(),
            Some(Token::Semicolon) => { self.next_token()?; Ok(()) }, // Empty statement
            Some(Token::Int) | Some(Token::Char) => {
                 // C99+ allows declarations anywhere, C4 original might not.
                 // For C4 compatibility, local declarations should ideally be parsed
                 // only at the start of a block/function.
                 // If we hit Int/Char here, it's likely an error unless we implement mid-block decl.
                 // Simplification: Error if declaration not at function start.
                 let ti = self.next_token()?;
                 Err(ParseError::Other(format!("Variable declarations only allowed at function start (found {:?} at {}:{})", ti.token, ti.line, ti.column)))
                 // If allowing mid-block, call parse_local_declaration here (needs max_offset_count context).
            }
            Some(Token::Eof) => Err(ParseError::UnexpectedToken { expected: "statement".into(), found: Token::Eof, line: self.current_pos().0, column: self.current_pos().1 }),
            None => Err(ParseError::UnexpectedToken { expected: "statement".into(), found: Token::Eof, line: self.current_pos().0, column: self.current_pos().1 }), // Should be caught earlier?
            _ => self.parse_expression_statement(), // Default to expression statement
        }
    }

    /// Parses an if (...) stmt [else stmt]
    fn parse_if_statement(&mut self) -> ParseResult<()> {
        self.expect_token(Token::If, "if keyword")?;
        self.expect_token(Token::LParen, "'(' after if")?;
        self.parse_expression_with_level(PREC_BOTTOM)?; // Condition -> AX
        self.expect_token(Token::RParen, "')' after if condition")?;

        // Emit branch if zero (condition false)
        let else_patch_loc = self.reserve_jump_operand(Instruction::Jz); // Jump if Zero

        // Parse 'then' block
        self.parse_statement()?;

        // Handle 'else'
        if self.consume_optional(Token::Else)? {
            // Have 'else', need to jump over it from end of 'then' block
            let end_patch_loc = self.reserve_jump_operand(Instruction::Jmp); // Jump over else

            // Patch the Jz to jump here (start of else block)
            let else_start_addr = self.current_code_addr();
             self.patch_jump_target(else_patch_loc, else_start_addr);

            // Parse 'else' block
            self.parse_statement()?;

            // Patch the JMP to jump here (end of else block)
            let end_addr = self.current_code_addr();
            self.patch_jump_target(end_patch_loc, end_addr);
        } else {
            // No 'else', patch the Jz to jump here (after 'then' block)
            let end_addr = self.current_code_addr();
            self.patch_jump_target(else_patch_loc, end_addr);
        }
        Ok(())
    }

    /// Parses while (...) stmt
    fn parse_while_statement(&mut self) -> ParseResult<()> {
        self.expect_token(Token::While, "while keyword")?;
        let loop_start_addr = self.current_code_addr(); // Address before condition

        self.expect_token(Token::LParen, "'(' after while")?;
        self.parse_expression_with_level(PREC_BOTTOM)?; // Condition -> AX
        self.expect_token(Token::RParen, "')' after while condition")?;

        // Emit branch if zero (condition false) to exit loop
        let exit_patch_loc = self.reserve_jump_operand(Instruction::Jz); // Jump if Zero

        // Parse loop body
        self.parse_statement()?;

        // Jump back to the start of the loop (before condition)
        self.emit_with_operand(Instruction::Jmp, loop_start_addr as Value);

        // Patch the Jz to jump here (after the loop body and jump back)
        let loop_end_addr = self.current_code_addr();
        self.patch_jump_target(exit_patch_loc, loop_end_addr);

        Ok(())
    }

     /// Parses return [expr];
    fn parse_return_statement(&mut self) -> ParseResult<()> {
        self.expect_token(Token::Return, "return keyword")?;
        if !self.consume_optional(Token::Semicolon)? {
            // Has return value
            self.parse_expression_with_level(PREC_BOTTOM)?; // Result -> AX
            // TODO: Check if expression type matches function return type
            self.expect_token(Token::Semicolon, "';' after return value")?;
        }
         // If no return value, AX might hold garbage, but C often ignores void returns.
         // Emit LEV to clean up stack frame and return
        self.emit(Instruction::Lev);
        Ok(())
    }

     /// Parses { [declarations] [statements] }
    fn parse_block_statement(&mut self) -> ParseResult<()> {
         self.expect_token(Token::LBrace, "'{' to start block")?;
         self.symbol_table.enter_scope();

         // Simplification: Assume declarations only allowed at function start for now.
         // If allowing mid-block declarations, would need to parse them here
         // using parse_local_declaration and manage max_bp_offset.

         // Parse statements within the block
         while self.peek_token()? != Some(&Token::RBrace) {
             if self.peek_token()? == Some(&Token::Eof) {
                 let (l, c) = self.current_pos();
                 return Err(ParseError::UnexpectedToken { expected: "'}' to end block".into(), found: Token::Eof, line: l, column: c });
             }
             self.parse_statement()?;
         }
         self.expect_token(Token::RBrace, "'}' to end block")?;
         self.symbol_table.leave_scope();
         Ok(())
    }

     /// Parses expression;
    fn parse_expression_statement(&mut self) -> ParseResult<()> {
        self.parse_expression_with_level(PREC_BOTTOM)?;
        // The result in AX is discarded for an expression statement.
        // C4 VM might require explicit pop if expression leaves value on stack?
        // Assume expression result is only in AX and doesn't need popping here.
        self.expect_token(Token::Semicolon, "';' after expression statement")?;
        Ok(())
    }

    // --- Expression Parsing (Operator Precedence Climbing) ---

    /// Parses an expression using operator precedence climbing.
    /// Entry point: parse_expression_with_level(PREC_ASSIGN) or PREC_BOTTOM
    fn parse_expression_with_level(&mut self, min_precedence: i32) -> ParseResult<ExprInfo> {
        // Parse the left-hand side (primary or unary)
        // Handle potential LValue loading deferral within parse_primary/postfix
        let mut left_info = self.parse_primary()?;

        loop {
            let current_token = match self.peek_token()? {
                Some(token) => token.clone(), // Clone token to check precedence
                None => break, // End of input
            };

            // --- FIX: Corrected line ---
            let precedence = self.get_operator_precedence(&current_token); // Pass reference to current_token

            // Stop if operator precedence is lower than current minimum, or not an operator
            if precedence < min_precedence || precedence == PREC_BOTTOM {
                break;
            }

            // --- Handle Binary Operator ---
            let op_token_info = self.next_token()?; // Consume the operator

            // If LHS was an LValue (address in AX), load its value now before PUSH,
            // unless the operator is assignment (=) or array index ([]).
            if left_info.is_lvalue && op_token_info.token != Token::Assign && op_token_info.token != Token::LBracket {
                 if let Some(load_instr) = left_info.lvalue_load_instr {
                    self.emit(load_instr); // LI / LC
                    left_info.is_lvalue = false; // Now it's the value (RValue)
                 } else {
                    // This shouldn't happen if is_lvalue is true
                    return Err(ParseError::Other("Internal error: LValue missing load instruction".into()));
                 }
            }


            // Special handling for ternary/conditional operator ?:
            if op_token_info.token == Token::Cond { // '?'
                 // Condition result is already in AX from left_info
                 let else_patch_loc = self.reserve_jump_operand(Instruction::Jz); // Branch if Zero

                // Parse the 'true' expression (result goes to AX)
                let _true_expr_info = self.parse_expression_with_level(PREC_COND)?;

                // Jump over the 'false' expression
                let end_patch_loc = self.reserve_jump_operand(Instruction::Jmp);

                // Patch Jz to jump here (start of 'false' expression)
                self.patch_jump_target(else_patch_loc, self.current_code_addr());

                self.expect_token(Token::Colon, "':' for conditional operator")?;

                 // Parse the 'false' expression (result goes to AX)
                let _false_expr_info = self.parse_expression_with_level(PREC_COND)?;

                // Patch JMP to jump here (after 'false' expression)
                self.patch_jump_target(end_patch_loc, self.current_code_addr());

                 // Type of conditional expr? Could be complex. Assume INT for simplicity like C4.
                 // Result is RValue.
                left_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
                continue; // Continue parsing loop, result is in AX
            }

            // Push LHS result/address before parsing RHS
            // If it was an LValue address for assignment, AX holds address.
            // If it was an RValue, AX holds value.
            self.emit(Instruction::Push);

            // Recursively parse the right-hand side with precedence + 1 (for left-associativity)
            // or precedence (for right-associativity like assignment)
            let next_min_precedence = if Self::is_right_associative(&op_token_info.token) {
                precedence
            } else {
                precedence + 1
            };
            let mut right_info = self.parse_expression_with_level(next_min_precedence)?;

            // If RHS was an LValue (address in AX), load its value now.
            if right_info.is_lvalue {
                 if let Some(load_instr) = right_info.lvalue_load_instr {
                    self.emit(load_instr); // LI / LC
                    right_info.is_lvalue = false; // Now it's the value (RValue)
                 } else {
                    return Err(ParseError::Other("Internal error: LValue missing load instruction".into()));
                 }
            }

            // --- Emit code for the binary operator ---
            // Stack has LHS (addr or value), AX has RHS value
            left_info = self.emit_binary_op_code(&op_token_info.token, left_info, right_info)?;

        } // End loop

        // Final check: If the overall expression result was an LValue address
        // (e.g., just a variable name `x;`) and it wasn't used by an operator
        // that consumes the address (like assignment), load the value now.
        if left_info.is_lvalue {
            if let Some(load_instr) = left_info.lvalue_load_instr {
                self.emit(load_instr); // LI / LC
                left_info.is_lvalue = false; // Now it's the value (RValue)
            }
         }


        Ok(left_info)
    }

     /// Gets the precedence level of a binary operator token.
    fn get_operator_precedence(&self, token: &Token) -> i32 {
        match token {
            Token::Assign => PREC_ASSIGN,
            Token::Cond => PREC_COND, // ?
            Token::Lor => PREC_LOR,
            Token::Lan => PREC_LAN,
            Token::BitOr => PREC_OR, // |
            Token::BitXor => PREC_XOR, // ^
            Token::BitAnd => PREC_AND, // & (binary)
            Token::Eq | Token::Ne => PREC_EQ,
            Token::Lt | Token::Gt | Token::Le | Token::Ge => PREC_REL,
            Token::Shl | Token::Shr => PREC_SHIFT,
            Token::Add | Token::Sub => PREC_ADD, // Binary + -
            Token::Mul | Token::Div | Token::Mod => PREC_MUL, // Binary * / %
            // Postfix ops handled separately, but precedence needed for climbing
            Token::LBracket => PREC_POSTFIX, // Array subscript []
            Token::LParen => PREC_POSTFIX,   // Function call ()
            Token::Inc | Token::Dec => PREC_POSTFIX, // Postfix ++/-- (handled in parse_primary)
            // Add other binary operators here
            _ => PREC_BOTTOM, // Not a binary operator we handle here
        }
    }
    // Helper for right-associative operators
     fn is_right_associative(token: &Token) -> bool {
        matches!(token, Token::Assign | Token::Cond)
     }

     /// Emits VM code for a binary operator.
     /// Assumes LHS (value or address) is on stack, RHS value is in AX.
     /// Result ends up in AX.
     fn emit_binary_op_code(&mut self, op: &Token, left: ExprInfo, right: ExprInfo) -> ParseResult<ExprInfo> {
        // Default result type is Int, LValue is false unless it's assignment
        let result_type; // Assign within each match arm - FIX: Remove mut
        let result_lvalue = false; // Binary ops usually produce RValues

        match op {
            // Assignment (=)
            Token::Assign => {
                 // Stack: [lhs_addr], AX: rhs_value
                 if !left.is_lvalue {
                     let (l, c) = self.current_pos(); // Position of the assignment op
                     return Err(ParseError::NotAssignable("expression".into(), l, c));
                 }
                 // Type of assignment is type of LHS
                result_type = left.data_type.clone();
                // Emit store (pops address, stores AX)
                self.emit_store(&result_type)?;
                 // Result of assignment in C is the assigned value (RValue), which is already in AX.
            }

            // Arithmetic (+, -, *, /, %)
            Token::Add => {
                 // Pointer arithmetic check
                 // Stack: [lhs_val], AX: rhs_val (or vice versa if types swapped)
                 if left.data_type.is_pointer() && right.data_type.is_pointer() {
                      let (l, c) = self.current_pos();
                      return Err(ParseError::TypeMismatch("Cannot add two pointers".into(), l, c));
                 } else if left.data_type.is_pointer() && right.data_type.is_int_or_char() {
                     // ptr + int: Scale int (in AX)
                     self.emit_pointer_scaling(left.data_type.deref().unwrap_or(DataType::Char)); // Scale int in AX
                     self.emit(Instruction::Add); // Add ptr (from stack) + scaled_int (AX) -> AX
                     result_type = left.data_type; // Result is pointer
                 } else if left.data_type.is_int_or_char() && right.data_type.is_pointer() {
                     // int + ptr: Swap required conceptually. VM might handle, or need PSH/POP
                     // Simple VM: Just add. Assume ptr + int order doesn't matter for VM ADD.
                     self.emit(Instruction::Add); // Add int (from stack) + ptr (AX) -> AX
                     result_type = right.data_type; // Result is pointer
                 } else { // Both INT/CHAR
                     self.emit(Instruction::Add);
                     result_type = DataType::Int; // Result is int
                 }
            }
            Token::Sub => {
                 // Pointer arithmetic check
                 // Stack: [lhs_val], AX: rhs_val
                 if left.data_type.is_pointer() && right.data_type.is_pointer() {
                     // ptr - ptr = int (difference scaled down)
                     self.emit(Instruction::Sub); // AX = ptr(stack) - ptr(AX)
                     self.emit_pointer_scaling_division(left.data_type.deref().unwrap_or(DataType::Char)); // Scale AX
                     result_type = DataType::Int;
                 } else if left.data_type.is_pointer() && right.data_type.is_int_or_char() {
                     // ptr - int = ptr
                     self.emit_pointer_scaling(left.data_type.deref().unwrap_or(DataType::Char)); // Scale int in AX
                     self.emit(Instruction::Sub); // AX = ptr(stack) - scaled_int(AX)
                     result_type = left.data_type;
                 } else if left.data_type.is_int_or_char() && right.data_type.is_pointer() {
                     let (l, c) = self.current_pos();
                     return Err(ParseError::TypeMismatch("Cannot subtract pointer from integer".into(), l, c));
                 } else { // Both INT/CHAR
                     self.emit(Instruction::Sub);
                     result_type = DataType::Int;
                 }
            }
            Token::Mul => { self.emit(Instruction::Mul); result_type = DataType::Int; }
            Token::Div => { self.emit(Instruction::Div); result_type = DataType::Int; }
            Token::Mod => { self.emit(Instruction::Mod); result_type = DataType::Int; }

             // Relational (==, !=, <, >, <=, >=) -> Result is always Int (0 or 1)
            Token::Eq => { self.emit(Instruction::Eq); result_type = DataType::Int; }
            Token::Ne => { self.emit(Instruction::Ne); result_type = DataType::Int; }
            Token::Lt => { self.emit(Instruction::Lt); result_type = DataType::Int; }
            Token::Gt => { self.emit(Instruction::Gt); result_type = DataType::Int; }
            Token::Le => { self.emit(Instruction::Le); result_type = DataType::Int; }
            Token::Ge => { self.emit(Instruction::Ge); result_type = DataType::Int; }

             // Bitwise (|, ^, &)
            Token::BitOr => { self.emit(Instruction::Or); result_type = DataType::Int; }
            Token::BitXor => { self.emit(Instruction::Xor); result_type = DataType::Int; }
            Token::BitAnd => { self.emit(Instruction::And); result_type = DataType::Int; }

             // Shift (<<, >>)
            Token::Shl => { self.emit(Instruction::Shl); result_type = DataType::Int; }
            Token::Shr => { self.emit(Instruction::Shr); result_type = DataType::Int; }

            // Logical (||, &&) - C4 uses short-circuiting via jumps.
            // This simple implementation is NOT short-circuiting.
            // TODO: Implement proper short-circuiting for || and && using Jz/Jnz/Jmp
            Token::Lor => {
                 // Simple (non-short-circuit): Convert both operands to 0/1 then OR?
                 // Or just OR bits and check if result is non-zero.
                 self.emit(Instruction::Or); // Combine bits
                 self.emit(Instruction::Push); // Push result
                 self.emit_with_operand(Instruction::Imm, 0); // Load 0
                 self.emit(Instruction::Ne); // AX = (result != 0) ? 1 : 0
                 result_type = DataType::Int;
             }
             Token::Lan => {
                 self.emit(Instruction::And); // Combine bits
                 self.emit(Instruction::Push); // Push result
                 self.emit_with_operand(Instruction::Imm, 0); // Load 0
                 self.emit(Instruction::Ne); // AX = (result != 0) ? 1 : 0
                 result_type = DataType::Int;
            }

             // Array Indexing ([) - Handled as a postfix operator in parse_primary. Should not reach here.
            Token::LBracket => {
                 return Err(ParseError::Other("Internal error: Array index operator reached binary handler".into()));
            }

            _ => return Err(ParseError::Other(format!("Unsupported binary operator: {:?}", op))),
        }

        Ok(ExprInfo { data_type: result_type, is_lvalue: result_lvalue, lvalue_load_instr: None }) // Result is RValue
     }

     /// Emits code to multiply value in AX by sizeof type, for pointer scaling.
     fn emit_pointer_scaling(&mut self, target_type: DataType) {
         let size = target_type.size();
         if size > 1 {
             self.emit(Instruction::Push); // Push index/offset (in AX)
             self.emit_with_operand(Instruction::Imm, size as Value); // Load size
             self.emit(Instruction::Mul); // AX = index * size
         }
         // If size is 1, no scaling needed
     }
      /// Emits code to divide value in AX by sizeof type, for pointer difference scaling.
     fn emit_pointer_scaling_division(&mut self, target_type: DataType) {
          let size = target_type.size();
          if size > 1 {
             self.emit(Instruction::Push); // Push difference (in AX)
             self.emit_with_operand(Instruction::Imm, size as Value); // Load size
             self.emit(Instruction::Div); // AX = difference / size
         }
     }


    /// Parses primary expressions: literals, identifiers (vars/calls), parentheses, unary ops, postfix ops.
    /// Handles deferred loading of LValues.
    fn parse_primary(&mut self) -> ParseResult<ExprInfo> {
        let token_info = self.next_token()?;
        let mut expr_info: ExprInfo;

        match token_info.token {
            Token::Number(val) => {
                 if val < i32::MIN as i64 || val > i32::MAX as i64 { /* Range check? */ }
                 self.emit_with_operand(Instruction::Imm, val as Value);
                 expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
            }
            Token::StringLiteral(s) => {
                 let addr = self.add_string_literal(&s);
                 self.emit_with_operand(Instruction::Imm, addr);
                 // String literals decay to char*
                 expr_info = ExprInfo { data_type: DataType::Ptr(Box::new(DataType::Char)), is_lvalue: false, lvalue_load_instr: None };
            }
             Token::CharLiteral(c) => {
                self.emit_with_operand(Instruction::Imm, c as Value);
                expr_info = ExprInfo { data_type: DataType::Char, is_lvalue: false, lvalue_load_instr: None };
            }
            Token::Ident(name) => {
                let sym = self.symbol_table.find(&name)
                    .ok_or_else(|| ParseError::UndefinedSymbol(name.clone(), token_info.line, token_info.column))?
                    .clone(); // Clone symbol data

                match sym.class {
                    SymbolClass::Num => { // Enum member
                        self.emit_with_operand(Instruction::Imm, sym.value as Value);
                        expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
                    }
                    SymbolClass::Loc | SymbolClass::Glo => {
                        // Emit address calculation -> AX
                        if sym.class == SymbolClass::Loc {
                            self.emit_with_operand(Instruction::Lea, sym.value as Value); // LEA bp+offset -> AX
                        } else { // Glo
                            self.emit_with_operand(Instruction::Imm, sym.value as Value); // IMM data_addr -> AX
                        }
                        // Now AX contains the address (LValue). Store the appropriate load instruction.
                         let load_instr = Some(if sym.data_type == DataType::Char { Instruction::Lc } else { Instruction::Li });
                         expr_info = ExprInfo { data_type: sym.data_type.clone(), is_lvalue: true, lvalue_load_instr: load_instr };
                        // **Defer the LI/LC**: Load only when used as RValue (handled by callers or end of parse_primary)
                    }
                    SymbolClass::Fun | SymbolClass::Sys => {
                        // Function name used directly yields address/ID.
                        if sym.value < i32::MIN as i64 || sym.value > i32::MAX as i64 { /* Range check */ }
                        self.emit_with_operand(Instruction::Imm, sym.value as Value);
                         // Represent function type? C4 doesn't have strong func types. Use Ptr(ReturnType).
                        expr_info = ExprInfo { data_type: DataType::Ptr(Box::new(sym.data_type)), is_lvalue: false, lvalue_load_instr: None };
                    }
                     SymbolClass::Enum => {
                        // Enum tag name used directly? Invalid in C4 expressions.
                         return Err(ParseError::Other(format!("Cannot use enum tag '{}' directly in expression at {}:{}", name, token_info.line, token_info.column)));
                    }
                    SymbolClass::Key => { // Should not happen if lexer is correct
                        return Err(ParseError::Other(format!("Unexpected keyword '{}' in expression at {}:{}", name, token_info.line, token_info.column)));
                    }
                }
            }
            Token::LParen => { // Parentheses or Cast
                // Peek to distinguish (type) from (expr)
                if matches!(self.peek_token()?, Some(Token::Int | Token::Char | Token::Void)) {
                    // Check if it's followed by '*' or ')' to confirm type cast intent
                    // Simple check: if it's a type keyword, assume cast for now.
                    expr_info = self.parse_cast_expression()? // Returns ExprInfo
                } else { // Grouping parentheses
                    expr_info = self.parse_expression_with_level(PREC_BOTTOM)?;
                    self.expect_token(Token::RParen, "')' to close parenthesis")?;
                    // Result type/lvalue status comes from inner expression
                    // Load if inner result was LValue and wasn't consumed
                     if expr_info.is_lvalue {
                         if let Some(load_instr) = expr_info.lvalue_load_instr {
                             self.emit(load_instr);
                             expr_info.is_lvalue = false;
                         }
                     }
                }
            }

            // --- Unary Operators ---
             Token::Sizeof => {
                self.expect_token(Token::LParen, "'(' after sizeof")?;
                 // Sizeof can take a type or an expression
                 // Peek to see if it's a type name
                 let size = if matches!(self.peek_token()?, Some(Token::Int | Token::Char | Token::Void)) {
                     // Assume sizeof(type)
                     let target_type = self.parse_type()?;
                     target_type.size()
                 } else {
                     // Assume sizeof(expression)
                     let expr_value_info = self.parse_expression_with_level(PREC_BOTTOM)?;
                     // We only need the type's size, the expression code is discarded
                     // C standard: sizeof(expr) doesn't evaluate expr.
                     expr_value_info.data_type.size()
                 };
                self.expect_token(Token::RParen, "')' after sizeof argument")?;
                 self.emit_with_operand(Instruction::Imm, size as Value);
                 expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
             }
             Token::Asterisk => { // Dereference (*)
                 // Parse operand first
                let inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                 // If operand was LValue address, load the value (which is the pointer)
                 let mut current_info = inner_info;
                 if current_info.is_lvalue {
                    if let Some(load_instr) = current_info.lvalue_load_instr {
                        self.emit(load_instr);
                        current_info.is_lvalue = false;
                    }
                 }
                 // Now AX holds the pointer value. Check if it's actually a pointer type.
                 if let DataType::Ptr(target_type) = current_info.data_type {
                     // AX holds the address *to be dereferenced*.
                     // The result *is* an LValue (the memory location itself).
                     // Store the load instruction for the *target* type.
                     let target_load_instr = Some(if *target_type == DataType::Char { Instruction::Lc } else { Instruction::Li });
                     expr_info = ExprInfo { data_type: *target_type, is_lvalue: true, lvalue_load_instr: target_load_instr };
                     // Defer the load: AX holds the address. Load only when needed.
                 } else {
                      return Err(ParseError::InvalidDereference(token_info.line, token_info.column));
                 }
             }
             Token::Ampersand => { // Address-of (&)
                 // Address-of requires an LValue as operand
                let inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                 if !inner_info.is_lvalue {
                     return Err(ParseError::InvalidAddressOf(token_info.line, token_info.column));
                 }
                 // inner_info evaluation should have left the *address* in AX (e.g., via LEA or IMM).
                 // The '&' operator means we *keep* that address. Do nothing except update type.
                expr_info = ExprInfo {
                    data_type: DataType::Ptr(Box::new(inner_info.data_type)),
                    is_lvalue: false, // Result of & is an RValue (the address value itself)
                    lvalue_load_instr: None,
                 };
             }
             Token::Not => { // Logical NOT (!)
                let mut inner_info = self.parse_expression_with_level(PREC_UNARY)?; // Expr -> AX (potentially address)
                // Load if LValue
                if inner_info.is_lvalue {
                    if let Some(load_instr) = inner_info.lvalue_load_instr { self.emit(load_instr); inner_info.is_lvalue = false; }
                }
                // Now AX has value
                self.emit_with_operand(Instruction::Imm, 0); // Load 0
                self.emit(Instruction::Eq); // AX = (AX == 0) ? 1 : 0
                expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
             }
            Token::BitNot => { // Bitwise NOT (~)
                let mut inner_info = self.parse_expression_with_level(PREC_UNARY)?; // Expr -> AX
                if inner_info.is_lvalue {
                    if let Some(load_instr) = inner_info.lvalue_load_instr { self.emit(load_instr); inner_info.is_lvalue = false; }
                }
                self.emit_with_operand(Instruction::Imm, -1); // Load -1 (all bits set)
                self.emit(Instruction::Xor); // AX = AX ^ -1
                expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None }; // Assume Int result
            }
            Token::Add => { // Unary Plus (+) - No-op semantically, just ensure value is loaded
                expr_info = self.parse_expression_with_level(PREC_UNARY)?;
                if expr_info.is_lvalue {
                    if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; }
                }
                // TODO: Perform integer promotion if type is char? C4 likely ignores.
            }
            Token::Sub => { // Unary Minus (-)
                expr_info = self.parse_expression_with_level(PREC_UNARY)?;
                if expr_info.is_lvalue {
                    if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; }
                }
                // TODO: Type check - must be arithmetic type
                self.emit(Instruction::Neg); // Negate value in AX
                expr_info.is_lvalue = false; // Result is rvalue
            }
            Token::Inc | Token::Dec => { // Prefix ++/--
                let op = token_info.token;
                // Operand must be LValue
                let inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                if !inner_info.is_lvalue { return Err(ParseError::NotAnLValue("prefix ++/-- operand".into(), token_info.line, token_info.column)); }
                let load_instr = inner_info.lvalue_load_instr.ok_or_else(|| ParseError::NotAnLValue("prefix ++/-- operand".into(), token_info.line, token_info.column))?;

                // AX contains address.
                self.emit(Instruction::Push); // push addr [S: addr]
                self.emit(load_instr);        // LI/LC -> AX = value
                self.emit(Instruction::Push); // push value [S: addr, value]
                self.emit_with_operand(Instruction::Imm, 1); // Load 1
                let element_type = inner_info.data_type.deref().unwrap_or(DataType::Char);
                let is_ptr = inner_info.data_type.is_pointer();
                if is_ptr { self.emit_pointer_scaling(element_type); } // Scale 1 if ptr
                self.emit(if op == Token::Inc { Instruction::Add } else { Instruction::Sub }); // AX = value +/- scaled_1 (new_value)
                // Store new_value into address. Needs addr on stack, value in AX.
                // Stack: [addr, value]. AX = new_value.
                self.emit_store(&inner_info.data_type)?; // Pops addr, stores AX (new_value). [S: value]
                // Result of prefix is the *new* value, which is already in AX.
                // Discard original value from stack.
                self.emit_with_operand(Instruction::Adj, 1); // Discard original value. [S: ]

                expr_info = ExprInfo { data_type: inner_info.data_type, is_lvalue: false, lvalue_load_instr: None }; // Result is rvalue
            }


            // --- Error Cases ---
             Token::Eof => return Err(ParseError::UnexpectedToken { expected: "primary expression".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
             _ => return Err(ParseError::UnexpectedToken { expected: "primary expression".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
        }

         // --- Handle Postfix Operators ---
         loop {
             match self.peek_token()? {
                 Some(Token::LBracket) => { // Array indexing a[i]
                     // Current expr_info holds base (e.g., pointer value or address of array)
                     // If it's an LValue address, load the pointer value first.
                     if expr_info.is_lvalue {
                         if let Some(load_instr) = expr_info.lvalue_load_instr {
                             self.emit(load_instr); // Load base pointer value -> AX
                             expr_info.is_lvalue = false;
                         }
                     }
                     // Now AX holds the base pointer value. Check type.
                      if !expr_info.data_type.is_pointer() {
                         let (l,c) = self.current_pos();
                         return Err(ParseError::TypeMismatch("Attempting to index non-pointer type".into(), l, c));
                      }
                      let element_type = expr_info.data_type.deref().unwrap_or(DataType::Char);

                     self.next_token()?; // Consume '['
                     self.emit(Instruction::Push); // Push base pointer value

                     // Parse index expression
                     let mut index_info = self.parse_expression_with_level(PREC_BOTTOM)?; // Index expr -> AX
                     // Load index value if it's an LValue address
                     if index_info.is_lvalue {
                        if let Some(load_instr) = index_info.lvalue_load_instr { self.emit(load_instr); index_info.is_lvalue = false; }
                     }
                     self.expect_token(Token::RBracket, "']' to close array index")?;

                     // AX has index value, stack has base ptr. Emit code for `base + index * scale`
                     self.emit_pointer_scaling(element_type.clone()); // Scale index in AX
                     self.emit(Instruction::Add); // AX = base_addr + scaled_index
                     // Result is the address of the element (LValue).
                     let target_load_instr = Some(if element_type == DataType::Char { Instruction::Lc } else { Instruction::Li });
                      expr_info = ExprInfo { data_type: element_type, is_lvalue: true, lvalue_load_instr: target_load_instr };
                      // Defer the load. AX holds the element's address.
                 }
                 Some(Token::LParen) => { // Function call f(...)
                    // Current expr_info holds function (address/ID or pointer value)
                    // Load if LValue (e.g. pointer to function variable)
                     if expr_info.is_lvalue {
                         if let Some(load_instr) = expr_info.lvalue_load_instr {
                             self.emit(load_instr); // Load function address/ID -> AX
                             expr_info.is_lvalue = false;
                         }
                     }
                     // Now AX holds function address/ID.
                    let func_type = expr_info.data_type; // Type of the expression yielding the function ptr/ID
                    // TODO: Check if func_type is actually callable? C4 is loose.

                    self.next_token()?; // Consume '('
                    self.emit(Instruction::Push); // Push func address/id
                     expr_info = self.parse_function_call_args(func_type)?; // Parses args, emits pushes, emits CALL, ADJ
                     // Result is RValue (return value in AX)
                 }
                  Some(Token::Inc | Token::Dec) => { // Postfix ++/--
                      let op_token = self.next_token()?.token; // Consume ++ or --
                      if !expr_info.is_lvalue { let (l,c) = self.current_pos(); return Err(ParseError::NotAnLValue("postfix ++/-- operand".into(), l, c)); }
                      let load_instr = expr_info.lvalue_load_instr.ok_or_else(|| { let (l,c) = self.current_pos(); ParseError::NotAnLValue("postfix ++/-- operand".into(), l, c)})?;

                      // AX has address
                      self.emit(Instruction::Push); // Push addr [S: addr]
                      self.emit(load_instr);        // LI/LC -> AX = value (original value)
                      self.emit(Instruction::Push); // Push value (this is the result) [S: addr, value]
                      self.emit_with_operand(Instruction::Imm, 1); // Load 1
                      let element_type = expr_info.data_type.deref().unwrap_or(DataType::Char);
                      let is_ptr = expr_info.data_type.is_pointer();
                      if is_ptr { self.emit_pointer_scaling(element_type.clone()); } // Scale 1 if ptr
                      // Add/Sub 1 to get new value. Stack: [addr, value], AX = scaled_1
                      self.emit(if op_token == Token::Inc { Instruction::Add } else { Instruction::Sub }); // AX = value +/- scaled_1 (new_value)
                      // Store new value back (pops addr from stack)
                      self.emit_store(&expr_info.data_type)?; // Stores AX into Mem[addr]. [S: value]

                      // Result is the *original* value, which is on the stack.
                      // Use C4 recalculation method (assuming no direct SWAP or POP AX):
                      self.emit(Instruction::Push); // Save new_value [S: value, new_value]
                      self.emit_with_operand(Instruction::Imm, 1);
                      if is_ptr { self.emit_pointer_scaling(element_type); }
                      // Apply opposite operation to new_value to get original value
                      self.emit(if op_token == Token::Inc { Instruction::Sub } else { Instruction::Add }); // AX = new_value -/+ scaled_1 = original_value
                      // Discard saved new_value and original value from stack
                      self.emit_with_operand(Instruction::Adj, 2); // Remove new_value and value [S: ]
                      // Now AX has original value.

                      expr_info.is_lvalue = false; // Result is rvalue
                  }
                 _ => break, // Not a postfix operator
             }
         } // End postfix loop

         // If the expression resulted in an LValue (address in AX) but wasn't consumed
         // by a postfix or upcoming binary operator requiring the address, load the value now.
        if expr_info.is_lvalue {
            // Check if the *next* token indicates a use that needs the address
            let needs_addr = match self.peek_token()? {
                Some(Token::Assign) => true, // Assignment needs address
                // Add other ops needing address? Compound assignment?
                _ => false,
            };
            if !needs_addr {
                if let Some(load_instr) = expr_info.lvalue_load_instr {
                    self.emit(load_instr); // LI / LC
                    expr_info.is_lvalue = false; // Now it's the value (RValue)
                }
            }
         }

        Ok(expr_info)
    }


    /// Parses function call arguments and emits CALL/ADJ.
    /// Assumes function address/syscall ID is on the stack.
    fn parse_function_call_args(&mut self, func_type: DataType) -> ParseResult<ExprInfo> {
         // func_type might be Ptr(ReturnType) or just the ReturnType if we add Func types
        let return_type = match func_type {
            DataType::Ptr(inner) => *inner, // Assume pointer to function returns inner type
            // If it's a built-in like printf, the symbol table gave its return type directly
            other => other,
        };

        let mut arg_count = 0;
        let mut arg_codes: Vec<Vec<Value>> = Vec::new(); // Store code for each arg L->R

        // Parse arguments L->R, storing generated code temporarily
        if self.peek_token()? != Some(&Token::RParen) {
            loop {
                 // Use a temporary buffer to capture code for this argument
                 // This avoids interleaving argument code if args have side effects
                 let mut temp_code_buffer: Vec<Value> = Vec::new();
                 let original_code = mem::take(&mut self.code); // Take ownership of main code
                 mem::swap(&mut self.code, &mut temp_code_buffer); // Put temp buffer in self.code

                 let mut arg_info = self.parse_expression_with_level(PREC_ASSIGN)?; // Parse one arg -> self.code (temp) has its code
                 // Load arg value if it's an LValue address
                 if arg_info.is_lvalue {
                     if let Some(load_instr) = arg_info.lvalue_load_instr {
                         // Emit load into temp buffer (self.code)
                         self.emit(load_instr);
                         arg_info.is_lvalue = false;
                     }
                 }
                 // TODO: Type check argument against function signature if available

                 mem::swap(&mut self.code, &mut temp_code_buffer); // Swap back, temp_code_buffer has arg code
                 self.code = original_code; // Restore original main code

                 arg_codes.push(temp_code_buffer); // Store the code generated for this arg
                 arg_count += 1;

                 if !self.consume_optional(Token::Comma)? { break; } // More args?
            }
        }
        self.expect_token(Token::RParen, "')' after function arguments")?;

        // Emit argument code and PUSH instructions in REVERSE order (C calling convention)
        for arg_code in arg_codes.iter().rev() {
            self.code.extend_from_slice(arg_code); // Emit code for arg calculation -> AX
            self.emit(Instruction::Push); // Push the result from AX
        }

        // Emit CALL (address/ID was pushed before parsing args)
        self.emit(Instruction::Call); // Pops address/ID, jumps, pushes return addr

        // Emit ADJ to clean up arguments from stack (caller cleanup)
        if arg_count > 0 {
            self.emit_with_operand(Instruction::Adj, arg_count as Value);
        }

        // Result is in AX, type is function return type
        Ok(ExprInfo { data_type: return_type, is_lvalue: false, lvalue_load_instr: None })
    }

    /// Parses a type specifier (int, char, void, pointers) used in casts or sizeof.
    fn parse_type(&mut self) -> ParseResult<DataType> {
        let base_type = match self.peek_token()? {
            Some(Token::Int) => { self.next_token()?; DataType::Int },
            Some(Token::Char) => { self.next_token()?; DataType::Char },
            Some(Token::Void) => { self.next_token()?; DataType::Void }, // If Void token exists
            _ => { let ti = self.next_token()?; return Err(ParseError::UnexpectedToken{expected:"type name (int, char, void)".into(), found:ti.token, line:ti.line, column:ti.column})}
        };
        self.parse_pointers(base_type) // Parse subsequent '*'
    }

     /// Parses a type cast expression `(type) expr`
    fn parse_cast_expression(&mut self) -> ParseResult<ExprInfo> {
        // LParen already consumed by caller (parse_primary)
        let target_type = self.parse_type()?;
        self.expect_token(Token::RParen, "')' after type cast")?;

        // Parse the expression being casted
        let mut inner_expr_info = self.parse_expression_with_level(PREC_UNARY)?;
        // Load value if LValue address
        if inner_expr_info.is_lvalue {
            if let Some(load_instr) = inner_expr_info.lvalue_load_instr {
                 self.emit(load_instr);
                 inner_expr_info.is_lvalue = false;
            }
        }

         // In C4/simple C, casting often doesn't generate code, just changes compiler's view.
         // No code emitted here, just update the type info. AX holds the value.
         // TODO: Implement potential checks (e.g., casting non-int/ptr to pointer is dubious).
        Ok(ExprInfo { data_type: target_type, is_lvalue: false, lvalue_load_instr: None }) // Cast result is rvalue
    }

    /// Helper to map symbol table errors to ParseError::Redeclaration or Other.
    /// Made static (associated function) to avoid borrow checker issues.
    fn map_add_error(symbol_error: String, name: &str, line: usize, column: usize) -> ParseError {
        // Check if the error message indicates a redeclaration (adjust string if needed)
        if symbol_error.contains("already defined") || symbol_error.contains("Redeclaration") {
            ParseError::Redeclaration(name.to_string(), line, column)
        } else {
            ParseError::Other(format!("Symbol table error for '{}' at {}:{}: {}", name, line, column, symbol_error))
        }
     }

} // impl Parser

// Add helper extension trait for DataType if not already in symbol.rs or vm.rs
trait DataTypeExt {
    fn is_pointer(&self) -> bool;
    fn is_int_or_char(&self) -> bool;
    fn size(&self) -> i64; // Get size in bytes
    fn size_in_words(&self) -> i64; // Get size in VM words (for offsets/ENT)
    fn deref(&self) -> Option<DataType>; // Get type pointed to
}

impl DataTypeExt for DataType {
    #[inline]
    fn is_pointer(&self) -> bool { matches!(self, DataType::Ptr(_)) }

    #[inline]
    fn is_int_or_char(&self) -> bool { matches!(self, DataType::Int | DataType::Char) }

    #[inline]
    fn size(&self) -> i64 {
        match self {
            DataType::Char => 1,
            // Assume Value is pointer-sized (e.g., 4 or 8 bytes)
            DataType::Int | DataType::Ptr(_) => std::mem::size_of::<Value>() as i64,
            DataType::Void => 0, // Or 1? C standard is ambiguous. C4 likely 0.
        }
    }
     #[inline]
    fn size_in_words(&self) -> i64 {
        match self {
            // C4 treats char as taking a full word slot on stack/locals for simplicity
            DataType::Char | DataType::Int | DataType::Ptr(_) => 1,
            DataType::Void => 0,
        }
    }
     #[inline]
     fn deref(&self) -> Option<DataType> {
         match self {
             DataType::Ptr(inner) => Some(*inner.clone()),
             _ => None,
         }
     }
}