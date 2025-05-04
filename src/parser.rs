//! Parser for a C4 subset, aiming for self-hosting capabilities.
//! Directly emits VM instructions.

// NOTE: Ensure you have added `#[derive(Clone)]` to your Lexer struct definition in lexer.rs!
use crate::lexer::{Lexer, Token, TokenInfo, LexerError};
use crate::symbol::{SymbolTable, SymbolClass, DataType};
// Make sure Instruction has TryFrom implemented and is accessible
// e.g., use crate::vm::{Instruction, Value, VmError}; // if TryFrom returns VmError
use crate::vm::{Instruction, Value, VALUE_SIZE_BYTES}; // Assuming TryFrom<Value> for Instruction is defined HERE
use std::iter::Peekable;
use std::mem; // Needed for mem::swap

// Define word size based on the VM's Value type (assuming i32 or i64)
const WORD_SIZE: usize = std::mem::size_of::<Value>();

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
const PREC_POW:    i32 = 13; // ** higher then mul/div
const PREC_UNARY:  i32 = 14; // ++ -- (prefix) * & ! ~ + - (unary) sizeof cast
const PREC_POSTFIX:i32 = 15; // () [] ++ -- (postfix)

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

// Assume system call opcodes are defined somewhere, maybe in vm.rs or a global consts mod
// These are PLACEHOLDERS - use the actual opcodes from your VM
const PRINTF_OPCODE: i64 = 37; // Example Prtf
const OPEN_OPCODE:   i64 = 30; // Example Open
const READ_OPCODE:   i64 = 31; // Example Read
const CLOSE_OPCODE:  i64 = 32; // Example Clos
const MALLOC_OPCODE: i64 = 33; // Example Malc
const FREE_OPCODE:   i64 = 34; // Example Free
const MEMSET_OPCODE: i64 = 35; // Example Mset
const MEMCMP_OPCODE: i64 = 36; // Example Mcmp
const EXIT_OPCODE:   i64 = 1;  // Example Exit

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut symbol_table = SymbolTable::new();

        // --- Add built-in keywords ---
        symbol_table.add_reserved("char", Token::Char).unwrap();
        symbol_table.add_reserved("else", Token::Else).unwrap();
        symbol_table.add_reserved("enum", Token::Enum).unwrap();
        symbol_table.add_reserved("if", Token::If).unwrap();
        symbol_table.add_reserved("int", Token::Int).unwrap();
        symbol_table.add_reserved("return", Token::Return).unwrap();
        symbol_table.add_reserved("sizeof", Token::Sizeof).unwrap();
        symbol_table.add_reserved("while", Token::While).unwrap();
        symbol_table.add_reserved("void", Token::Void).unwrap();

        // --- Add built-in functions (Syscalls) ---
        // Ensure all 5 arguments are provided to `add`:
        // name: String, token: Token, class: SymbolClass, data_type: DataType, value: i64
        // Use the correct Instruction opcodes from vm.rs
        symbol_table.add("printf".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, Instruction::Prtf as i64).expect("Failed to add built-in printf");
        symbol_table.add("open".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, Instruction::Open as i64).expect("Failed to add built-in open");
        symbol_table.add("read".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, Instruction::Read as i64).expect("Failed to add built-in read"); // ssize_t -> Int
        symbol_table.add("close".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, Instruction::Clos as i64).expect("Failed to add built-in close");
        symbol_table.add("malloc".to_string(), Token::Sys, SymbolClass::Sys, DataType::Ptr(Box::new(DataType::Void)), Instruction::Malc as i64).expect("Failed to add built-in malloc");
        symbol_table.add("free".to_string(), Token::Sys, SymbolClass::Sys, DataType::Void, Instruction::Free as i64).expect("Failed to add built-in free");
        symbol_table.add("memset".to_string(), Token::Sys, SymbolClass::Sys, DataType::Ptr(Box::new(DataType::Void)), Instruction::Mset as i64).expect("Failed to add built-in memset"); // Returns ptr
        symbol_table.add("memcmp".to_string(), Token::Sys, SymbolClass::Sys, DataType::Int, Instruction::Mcmp as i64).expect("Failed to add built-in memcmp");
        symbol_table.add("exit".to_string(), Token::Sys, SymbolClass::Sys, DataType::Void, Instruction::Exit as i64).expect("Failed to add built-in exit"); // Use VM Exit opcode value


        // --- Initialize data_segment WITH PADDING ---
        let initial_padding_size = VALUE_SIZE_BYTES; // Use size from vm.rs
        let initial_data_segment = vec![0u8; initial_padding_size];


        Parser {
            tokens: lexer.peekable(),
            symbol_table,
            code: Vec::new(),
            data_segment: initial_data_segment, // Starts with padding
            current_token_info: None,
            current_bp_offset: 0,
            ent_patch_location: None,
        }
    }

    // --- Token Helpers (Keep existing or refine) ---
    #[inline]
    fn next_token(&mut self) -> ParseResult<TokenInfo> {
         match self.tokens.next() {
            Some(Ok(token_info)) => {
                // Keep track of the *last successfully consumed* token's info
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
        // Use the stored last consumed token's position
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

    // Helper to calculate padding needed to reach alignment
    #[inline]
    fn calculate_padding(current_len: usize, alignment: usize) -> usize {
        if alignment == 0 { return 0; } // Avoid division by zero
        let remainder = current_len % alignment;
        if remainder == 0 { 0 } else { alignment - remainder }
    }

    /// Adds a string literal to the data segment.
    /// Returns the starting BYTE OFFSET of the literal within the data segment.
    /// Adds padding AFTER the null terminator to ensure subsequent data (like globals)
    /// can be word-aligned if needed.
    #[inline]
    fn add_string_literal(&mut self, literal: &str) -> Value {
        // Starting byte offset of this string (relative to data_segment start)
        // This will now start at initial_padding_size (e.g., 4) for the first string.
        let start_address = self.data_segment.len();

        self.data_segment.extend_from_slice(literal.as_bytes());
        self.data_segment.push(0); // Null-terminate

        // Add padding *after* the string to align the *next* item to a word boundary.
        let current_len = self.data_segment.len();
        // Use VALUE_SIZE_BYTES for alignment
        let padding = Self::calculate_padding(current_len, VALUE_SIZE_BYTES);
        self.data_segment.extend(std::iter::repeat(0).take(padding));

        if start_address > i32::MAX as usize {
            panic!("Data segment address exceeds i32::MAX!");
        }
        start_address as Value // Return the starting byte offset (which is now >= initial_padding_size)
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


// --- Update Global Variable Allocation ---
fn parse_global_declaration(&mut self, is_function_hint: bool) -> ParseResult<()> {
    let base_type: DataType;
    // Store start info in case of error reporting, mark unused for now
    let _start_info = self.current_token_info.clone();

    // --- Parse Base Type or Enum ---
    // Check for 'enum' first
    if self.peek_token()? == Some(&Token::Enum) {
         // It's an enum definition like `enum { ... };` or `enum Name { ... };`
         // This handles the entire enum definition and semicolon if present.
         self.parse_enum_declaration()?;
         // After parsing the enum, the line is consumed. No further parsing needed here.
         return Ok(());
    } else if self.peek_token()? == Some(&Token::Int) {
         self.next_token()?; // Consume 'int'
         base_type = DataType::Int;
    } else if self.peek_token()? == Some(&Token::Char) {
         self.next_token()?; // Consume 'char'
         base_type = DataType::Char;
    } else if self.peek_token()? == Some(&Token::Void) {
         self.next_token()?; // Consume 'void'
         base_type = DataType::Void;
    } else {
         // Invalid start for a global declaration
         let token_info = self.next_token()?; // Consume the unexpected token for error reporting
         return Err(ParseError::UnexpectedToken {
             expected: "global declaration (type or enum)".into(),
             found: token_info.token,
             line: token_info.line,
             column: token_info.column
         });
    }

    // --- Parse potential variable/function list (e.g., int a, *b, func();) ---
    loop {
        // Parse potential '*' pointers after the base type
        let current_type = self.parse_pointers(base_type.clone())?;

        // Expect an identifier for the variable or function name
        let (name, name_info) = self.expect_identifier("variable or function name")?;

        // --- Check if Function or Variable based on lookahead ---
         if self.peek_token()? == Some(&Token::LParen) {
             // It looks like a function definition or declaration `type name (...)`
             if !is_function_hint && self.symbol_table.get_current_scope() != 0 {
                 // This condition seems unlikely here as parse_global_declaration
                 // should only be called at scope 0. Included for robustness.
                 eprintln!("Warning: Function definition like syntax found within non-global scope at {}:{}", name_info.line, name_info.column);
             }

             // Check for disallowed function types (like void*) - adjust as needed for your C subset
             if current_type == DataType::Void && !matches!(base_type, DataType::Void) {
                 return Err(ParseError::Other(format!("Function '{}' cannot return void pointer at {}:{}", name, name_info.line, name_info.column)));
             }

             // It's a function definition (assuming declarations aren't separately handled)
             // parse_function_definition handles adding to symbol table, parsing args/body
             self.parse_function_definition(name, name_info, current_type)?;

             // A function definition consumes the rest of the declaration line (params, body).
             // We don't expect a comma or semicolon after the function body's '}'.
             return Ok(()); // Exit parse_global_declaration successfully

         } else {
              // It looks like a variable declaration `type name [= init];` or `type name, ...;`
              if is_function_hint {
                 // If the earlier peek suggested a function, but we didn't find '(', maybe log a warning.
                 eprintln!("Note: Expected function hint for '{}' but found variable syntax at {}:{}", name, name_info.line, name_info.column);
             }

             // Check for disallowed variable types (like void)
             if current_type == DataType::Void {
                 return Err(ParseError::Other(format!("Cannot declare variable '{}' of type void at {}:{}", name, name_info.line, name_info.column)));
             }

             // --- Global Variable Declaration Logic ---
             // Calculate size and alignment needed for this variable.
             let size_bytes = current_type.size() as usize;
             let alignment = current_type.alignment();

             // Calculate padding needed *before* this variable in the data segment
             // based on the current length and the variable's alignment requirement.
             let current_data_len = self.data_segment.len();
             let padding = Self::calculate_padding(current_data_len, alignment);
             self.data_segment.extend(std::iter::repeat(0).take(padding)); // Add padding bytes

             // The starting byte offset is the length *after* adding padding.
             // This offset is relative to the start of the `data_segment` Vec<u8>.
             let start_byte_offset = self.data_segment.len();

             // Check if the calculated offset exceeds the VM's address space (using i32::MAX)
             if start_byte_offset > i32::MAX as usize {
                 return Err(ParseError::Other(format!("Global variable '{}' offset exceeds address space at {}:{}", name, name_info.line, name_info.column)));
             }

             // Add the global variable symbol to the symbol table (scope 0).
             // Store the calculated *relative byte offset* as its 'value'.
             let _sym = self.symbol_table.add(
                 name.clone(),
                 Token::Ident(name.clone()), // Store the token if needed, or just name
                 SymbolClass::Glo,          // It's a global variable
                 current_type.clone(),      // Its data type
                 start_byte_offset as i64   // Store its relative byte offset
             ).map_err(|e| Parser::map_add_error(e, &name, name_info.line, name_info.column))?; // Map potential redeclaration error

             // Reserve space for the variable itself in the data segment by adding zeros.
             self.data_segment.extend(std::iter::repeat(0).take(size_bytes));
             // --- End Global Variable Declaration Logic ---

             // Handle potential initializers if supported for globals
             // C4 doesn't support complex global initializers.
             if self.consume_optional(Token::Assign)? {
                 // If you were to support constant initializers:
                 // 1. Parse the constant expression (e.g., number, char literal).
                 // 2. Get the resulting constant value.
                 // 3. Write the value directly into the `data_segment` at `start_byte_offset`.
                 //    This requires careful handling of byte order and size.
                 // For now, disallow initializers.
                 return Err(ParseError::Other(format!("Global initializers are not supported at {}:{}", name_info.line, name_info.column)));
             }
         } // End of variable handling block

        // After parsing one variable or function, check for ',' or ';'
        if self.consume_optional(Token::Comma)? {
            // Found a comma, continue the loop to parse the next variable in the list
            // e.g., int a, b, c;
            continue;
        } else {
            // Expect a semicolon to end the declaration list
            // e.g., int a; or int a, b;
            self.expect_token(Token::Semicolon, "';' or ',' after global declaration")?;
            // Found the semicolon, this declaration line is finished.
            return Ok(());
        }
    } // End loop for vars/funcs on one line (e.g. int a, b;)
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

                // Get the next token, expecting a number literal
                let val_token_info = self.next_token()?; // Consume the token after '=' or '-'

                // Check if the consumed token is a Number variant
                if let Token::Number(mut num_val) = val_token_info.token {
                    if negative {
                        // Apply negation, checking for overflow
                        num_val = num_val.checked_neg().ok_or_else(|| ParseError::Other(format!(
                            "Enum value negation overflow for '{}' at {}:{}",
                            name, // Use the name parsed earlier
                            val_token_info.line, val_token_info.column
                        )))?;
                    }
                    current_enum_value = num_val;
                } else {
                    // Token after '=' or '-' was not a number
                    return Err(ParseError::UnexpectedToken {
                        expected: "integer value for enum member".to_string(),
                        found: val_token_info.token, // Report the token we actually found
                        line: val_token_info.line,
                        column: val_token_info.column,
                    });
                }
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
                    Some(Token::Void) => {
                         // Check if 'void' is the only param and no name follows
                         self.next_token()?; // Consume void
                         if self.peek_token()? == Some(&Token::RParen) && param_count == 0 {
                              // This is `void func(void)` - valid, break loop
                             break;
                         } else {
                             let (l, c) = self.current_pos();
                              return Err(ParseError::Other(format!("'void' must be the only parameter and cannot have a name at {}:{}", l, c)));
                         }
                     },
                    _ => { let ti = self.next_token()?; return Err(ParseError::UnexpectedToken { expected:"parameter type".into(), found:ti.token, line:ti.line, column:ti.column }); }
                 };
                 let param_type = self.parse_pointers(base_type)?;

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
        // Check the *actual last instruction*, not just the value (in case LEV takes an operand later?)
        let mut needs_lev = true;
        if let Some(&last_instr_val) = self.code.last() {
             if Instruction::try_from(last_instr_val).map_or(false, |instr| instr == Instruction::Lev) {
                needs_lev = false;
            }
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
            // Stick to word offsets like C4 for simplicity.
            let size_in_words = var_type.size_in_words(); // Use size_in_words from symbol.rs
            if size_in_words <= 0 {
                // Avoid issues with void or zero-sized types if they somehow get here
                 return Err(ParseError::Other(format!("Invalid size for local variable '{}' at {}:{}", name, name_info.line, name_info.column)));
            }
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
                let mut init_info = self.parse_expression_with_level(PREC_ASSIGN)?; // Changed var name
                // Load value if initializer was LValue
                if init_info.is_lvalue {
                    if let Some(load_instr) = init_info.lvalue_load_instr {
                        self.emit(load_instr);
                        init_info.is_lvalue = false;
                        init_info.lvalue_load_instr = None; // Mark as loaded
                    } else {
                         // This shouldn't happen if is_lvalue is true for an initializer target
                         return Err(ParseError::Other("Internal error: LValue initializer missing load instruction".into()));
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
    
            // Parse condition expression -> AX (might hold address)
            let mut cond_info = self.parse_expression_with_level(PREC_BOTTOM)?;
    
            // *** ADDED: Load condition value if it's an LValue ***
            if cond_info.is_lvalue {
                if let Some(load_instr) = cond_info.lvalue_load_instr {
                    self.emit(load_instr); // Load the value from address in AX
                    // cond_info.is_lvalue = false; // State update not critical here
                } else {
                     // Error if it claims LValue but has no way to load
                     let (l, c) = self.current_pos();
                     return Err(ParseError::Other(format!("Internal error: If Condition LValue missing load instruction near {}:{}", l, c)));
                }
            }
            // *** END ADDED BLOCK ***
    
            self.expect_token(Token::RParen, "')' after if condition")?;
    
            // Emit branch if zero (condition false, tests value in AX)
            let else_patch_loc = self.reserve_jump_operand(Instruction::Jz);
    
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
    
            // Parse condition expression -> AX (might hold address)
            let mut cond_info = self.parse_expression_with_level(PREC_BOTTOM)?;
    
            // *** ADDED: Load condition value if it's an LValue ***
            if cond_info.is_lvalue {
                if let Some(load_instr) = cond_info.lvalue_load_instr {
                    self.emit(load_instr); // Load the value from address in AX
                    // cond_info.is_lvalue = false; // State update not critical here
                } else {
                     let (l, c) = self.current_pos();
                     return Err(ParseError::Other(format!("Internal error: While Condition LValue missing load instruction near {}:{}", l, c)));
                }
            }
            // *** END ADDED BLOCK ***
    
            self.expect_token(Token::RParen, "')' after while condition")?;
    
            // Emit branch if zero (condition false, tests value in AX) to exit loop
            let exit_patch_loc = self.reserve_jump_operand(Instruction::Jz);
    
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
                // Parse expression -> AX (might hold address)
                let mut ret_info = self.parse_expression_with_level(PREC_BOTTOM)?;
    
                // *** ADDED: Load return value if it's an LValue ***
                if ret_info.is_lvalue {
                    if let Some(load_instr) = ret_info.lvalue_load_instr {
                        self.emit(load_instr); // Load the value from address in AX
                        // ret_info.is_lvalue = false; // State update not critical here
                    } else {
                        let (l, c) = self.current_pos();
                        return Err(ParseError::Other(format!("Internal error: Return LValue missing load instruction near {}:{}", l, c)));
                    }
                }
                // *** END ADDED BLOCK ***
    
                // TODO: Check if expression type matches function return type
                self.expect_token(Token::Semicolon, "';' after return value")?;
            }
             // If no return value, AX might hold garbage, but C often ignores void returns.
             // Emit LEV to clean up stack frame and return (uses value currently in AX)
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
        // Parse expression -> AX (might hold address or value)
        let _expr_info = self.parse_expression_with_level(PREC_BOTTOM)?; // Use _expr_info as we don't use it

        // The result in AX is discarded for an expression statement.
        // We intentionally DO NOT load the value if it was an LValue here,
        // as the value itself is not used. Any side effects should have
        // happened during the parsing of the expression itself.

        self.expect_token(Token::Semicolon, "';' after expression statement")?;
        Ok(())
    }

    // --- Expression Parsing (Operator Precedence Climbing) ---

    /// Parses an expression using operator precedence climbing.
    /// Entry point: parse_expression_with_level(PREC_ASSIGN) or PREC_BOTTOM
    /// Parses an expression using operator precedence climbing.
    /// Entry point: parse_expression_with_level(PREC_ASSIGN) or PREC_BOTTOM
    /// Returns ExprInfo indicating if the result in AX is an LValue (address) or RValue.
    /// Callers are responsible for emitting load instructions if they need the value from an LValue result.
    fn parse_expression_with_level(&mut self, min_precedence: i32) -> ParseResult<ExprInfo> {
        // Parse the left-hand side (primary or unary, handled by parse_primary)
        let mut left_info = self.parse_primary()?; // parse_primary calls parse_postfix_operations internally

        loop { // Loop for binary operators
            // 1. Peek *first* to check the precedence. Borrow ends after this call.
            let maybe_precedence = self.peek_next_op_precedence(min_precedence)?;

            // 2. Decide whether to continue based on the peeked precedence
            if let Some(precedence) = maybe_precedence {
                // --- Precedence sufficient, now consume the operator ---
                // 3. Consume the token. This is a *new* mutable borrow, safe now.
                let op_token_info = self.next_token()?;

                // --- Sanity check (optional but good) ---
                let current_op_prec = Parser::get_operator_precedence(&op_token_info.token);
                if current_op_prec != precedence {
                     panic!(
                         "Internal parser error: Precedence mismatch after peek. Expected {}, found {} for {:?}",
                         precedence, current_op_prec, op_token_info.token
                    );
                }

                // --- Load LHS LValue if needed (before push/ternary) ---
                // Exception: Assignment operator *needs* the LValue address.
                // Other operators generally need the value.
                if left_info.is_lvalue && op_token_info.token != Token::Assign {
                    if let Some(load_instr) = left_info.lvalue_load_instr {
                        self.emit(load_instr);
                        left_info.is_lvalue = false; // It's now loaded (an RValue in AX)
                        left_info.lvalue_load_instr = None;
                    } else {
                        let (l, c) = (op_token_info.line, op_token_info.column);
                        return Err(ParseError::Other(format!("Internal error: LValue missing load instruction before binary op {:?} at {}:{}", op_token_info.token, l, c)));
                    }
                }

                // --- Handle Ternary ---
                if op_token_info.token == Token::Cond { // '?'
                    // AX has condition result (already loaded if it was LValue by the check above)
                    let else_patch_loc = self.reserve_jump_operand(Instruction::Jz);
                    // Parse the 'true' expression. Result -> AX (might be addr)
                    let mut true_expr_info = self.parse_expression_with_level(PREC_COND)?;
                    // Load result if LValue
                    if true_expr_info.is_lvalue {
                         if let Some(load_instr) = true_expr_info.lvalue_load_instr {
                             self.emit(load_instr);
                             true_expr_info.is_lvalue = false; // Ensure state is updated
                             true_expr_info.lvalue_load_instr = None;
                         } else {
                            let (l, c) = self.current_pos();
                            return Err(ParseError::Other(format!("Internal error: LValue true-expr missing load instruction near {}:{}", l, c)));
                        }
                    }
                    // Jump over 'false' expression if condition was true
                    let end_patch_loc = self.reserve_jump_operand(Instruction::Jmp);
                    // Patch Jz to land here (start of 'false' expression)
                    self.patch_jump_target(else_patch_loc, self.current_code_addr());
                    // Parse the ':'
                    self.expect_token(Token::Colon, "':' for conditional operator")?;
                    // Parse the 'false' expression. Result -> AX (might be addr)
                    let mut false_expr_info = self.parse_expression_with_level(PREC_COND)?;
                    // Load result if LValue
                    if false_expr_info.is_lvalue {
                         if let Some(load_instr) = false_expr_info.lvalue_load_instr {
                            self.emit(load_instr);
                            false_expr_info.is_lvalue = false; // Ensure state is updated
                            false_expr_info.lvalue_load_instr = None;
                         } else {
                            let (l, c) = self.current_pos();
                            return Err(ParseError::Other(format!("Internal error: LValue false-expr missing load instruction near {}:{}", l, c)));
                        }
                    }
                    // Patch Jmp to land here (end of conditional)
                    self.patch_jump_target(end_patch_loc, self.current_code_addr());
                    // Result type is usually Int or promotes; C4 is simple, assume Int. LValue is false.
                    // TODO: Type check - types of true/false branches should be compatible.
                    left_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
                    continue; // Important: Continue the outer loop after handling ternary
                 }

                // --- Push LHS, Parse RHS (For non-ternary binary operators) ---
                // LHS is in AX (either value, or address for '=')
                self.emit(Instruction::Push); // Push LHS (value or address) onto stack
                let next_min_precedence = if Self::is_right_associative(&op_token_info.token) {
                     precedence
                } else {
                     precedence + 1
                };
                // Parse the RHS. Result -> AX (might be addr)
                let mut right_info = self.parse_expression_with_level(next_min_precedence)?;

                // --- Load RHS LValue ---
                // The RHS of a binary operator is always needed as a value.
                if right_info.is_lvalue {
                    if let Some(load_instr) = right_info.lvalue_load_instr {
                        self.emit(load_instr);
                        right_info.is_lvalue = false; // Now loaded
                        right_info.lvalue_load_instr = None;
                    } else {
                        let (l, c) = self.current_pos();
                        return Err(ParseError::Other(format!("Internal error: LValue RHS missing load instruction near {}:{}", l, c)));
                    }
                 }

                // --- Emit Binary Op Code ---
                // emit_binary_op_code takes LHS info (mainly for '=' check and pointer type) and RHS info (type)
                // Stack: [LHS (val or addr)], AX: RHS value
                // Result will be placed in AX. left_info updated with result type/lvalue status.
                left_info = self.emit_binary_op_code(&op_token_info.token, left_info, right_info)?;
                // Loop continues to check next operator precedence

            } else {
                // 4. Peeked operator precedence was too low, or no operator -> break loop
                break;
            }
        } // End binary operator loop

        // --- NO FINAL LVALUE LOAD HERE ---
        // The caller is responsible for loading if the value is needed.

        Ok(left_info) // Return the final expression info (AX may hold address or value)
    }

     /// Gets the precedence level of a binary operator token.
     fn get_operator_precedence(token: &Token) -> i32 {
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
            Token::Mul | Token::Div | Token::Mod => PREC_MUL,Token::Pow => PREC_POW, // Binary * / %
            // Postfix ops handled separately by parse_postfix_operations
            _ => PREC_BOTTOM, // Not a binary operator we handle here
        }
    }
    // Helper for right-associative operators
     fn is_right_associative(token: &Token) -> bool {
        matches!(token, Token::Assign | Token::Cond | Token::Pow)
     }

     /// Emits VM code for a binary operator.
     /// Assumes LHS (value or address) is on stack, RHS value is in AX.
     /// Result ends up in AX.
     /// Updates and returns the ExprInfo reflecting the result.
     fn emit_binary_op_code(&mut self, op: &Token, left: ExprInfo, right: ExprInfo) -> ParseResult<ExprInfo> {
        // Default result type is Int, LValue is false unless it's assignment
        let result_type;
        let result_lvalue = false; // Only true if the op *results* in an LValue (uncommon for binary)

        match op {
            // Assignment (=)
            Token::Assign => {
                 // Stack: [lhs_addr], AX: rhs_value
                 if !left.is_lvalue {
                     let (l, c) = self.current_pos(); // Position of the assignment op
                     return Err(ParseError::NotAssignable("left side of assignment".into(), l, c));
                 }
                 // Type of assignment is type of LHS
                result_type = left.data_type.clone();
                // Emit store (pops address, stores AX)
                self.emit_store(&result_type)?;
                 // Result of assignment in C is the assigned value (RValue), which is already in AX.
                 // No need to update result_lvalue, it stays false.
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
                     // Stack: [ptr_val], AX: int_val
                     self.emit_pointer_scaling(left.data_type.deref().unwrap_or(DataType::Char)); // Scale int in AX by size of pointed-to type
                     self.emit(Instruction::Add); // Add ptr (from stack) + scaled_int (AX) -> AX
                     result_type = left.data_type; // Result is pointer
                 } else if left.data_type.is_int_or_char() && right.data_type.is_pointer() {
                     // int + ptr: Need to scale the int (on stack) before adding.
                     // Stack: [int_val], AX: ptr_val
                     // Use PUSH/IMM 0/ADD sequence twice to swap AX and stack top without Swap/PopAx
                     self.emit(Instruction::Push); // Push ptr_val. Stack: [int_val, ptr_val]
                     self.emit_with_operand(Instruction::Imm, 0); // AX = 0
                     self.emit(Instruction::Add); // AX = ptr_val + 0 = ptr_val. Stack: [int_val] (Pops ptr_val)
                     self.emit(Instruction::Push); // Push ptr_val. Stack: [int_val, ptr_val] (Put it back)
                     self.emit_with_operand(Instruction::Imm, 0); // AX = 0
                     self.emit(Instruction::Add); // AX = int_val + 0 = int_val. Stack: [ptr_val] (Pops int_val)
                     // Now AX has int_val, Stack has ptr_val

                     self.emit_pointer_scaling(right.data_type.deref().unwrap_or(DataType::Char)); // Scale int in AX by element size
                     self.emit(Instruction::Add); // Add ptr (from stack) + scaled_int (AX) -> AX
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
                     self.emit_pointer_scaling_division(left.data_type.deref().unwrap_or(DataType::Char)); // Scale AX (result) down
                     result_type = DataType::Int;
                 } else if left.data_type.is_pointer() && right.data_type.is_int_or_char() {
                     // ptr - int = ptr
                     // Stack: [ptr_val], AX: int_val
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

            Token::Pow => {
                // Stack: [lhs_val], AX: rhs_val (exponent)
                // Ensure both operands are integer types for basic exponentiation
                if !left.data_type.is_int_or_char() || !right.data_type.is_int_or_char() {
                    let (l, c) = self.current_pos();
                    return Err(ParseError::TypeMismatch("Operands for '**' must be integer types".into(), l, c));
                }
                // Assume VM has a Pow instruction
                self.emit(Instruction::Pow);
                result_type = DataType::Int; // Result is typically Int
                // Result is RValue
            }

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

            // Logical (||, &&) - Non-short-circuiting for now
            Token::Lor => {
                 // Stack: [lhs_val], AX: rhs_val
                 // C4 implements non-short-circuiting || as (lhs | rhs) != 0
                 self.emit(Instruction::Or); // AX = lhs | rhs
                 self.emit(Instruction::Push); // Push result
                 self.emit_with_operand(Instruction::Imm, 0); // Load 0
                 self.emit(Instruction::Ne); // AX = (result != 0) ? 1 : 0
                 result_type = DataType::Int;
            }
            Token::Lan => {
                 // Stack: [lhs_val], AX: rhs_val
                 // C4 implements non-short-circuiting && as (lhs & rhs) != 0 ???
                 // More accurately, C bool logic is (lhs != 0) && (rhs != 0)
                 // Simple (but maybe incorrect) C4 version: (lhs && rhs) != 0 equivalent?
                 // Let's do (lhs != 0) + (rhs != 0) == 2 ? No, that's not right.
                 // Let's try the bitwise `And` then check != 0, similar to `Or`
                 // This isn't semantically correct C, but might match C4 target.
                 // Correct C: Convert both to 0/1, then AND.
                 // AX = rhs != 0 ? 1 : 0
                 self.emit(Instruction::Push); // Push rhs
                 self.emit_with_operand(Instruction::Imm, 0);
                 self.emit(Instruction::Ne); // AX = rhs != 0 ? 1 : 0
                 // Stack: [lhs_val, rhs_bool]
                 // Swap using temp calculation
                 self.emit(Instruction::Push); // [lhs_val, rhs_bool, rhs_bool_ax]
                 self.emit_with_operand(Instruction::Imm, 0);
                 self.emit(Instruction::Add); // AX = rhs_bool; Stack: [lhs_val, rhs_bool]
                 self.emit(Instruction::Push); // [lhs_val, rhs_bool, rhs_bool_ax]
                 self.emit_with_operand(Instruction::Imm, 0);
                 self.emit(Instruction::Add); // AX = lhs_val; Stack: [rhs_bool]

                 // Now AX = lhs_val
                 self.emit(Instruction::Push); // Push lhs_val
                 self.emit_with_operand(Instruction::Imm, 0);
                 self.emit(Instruction::Ne); // AX = lhs != 0 ? 1 : 0
                 // Stack: [rhs_bool]

                 // Now AX = lhs_bool
                 self.emit(Instruction::And); // AX = lhs_bool & rhs_bool
                 result_type = DataType::Int;
            }


            _ => return Err(ParseError::Other(format!("Unsupported binary operator: {:?}", op))),
        }

        // Result is always RValue except for ops that might specifically return LValues (none here)
        Ok(ExprInfo { data_type: result_type, is_lvalue: result_lvalue, lvalue_load_instr: None })
     }

     /// Emits code to multiply value in AX by sizeof type, for pointer scaling.
     fn emit_pointer_scaling(&mut self, target_type: DataType) {
         let size = target_type.size(); // Size in bytes
         if size > 1 {
             self.emit(Instruction::Push); // Push index/offset (in AX)
             self.emit_with_operand(Instruction::Imm, size as Value); // Load size in bytes
             self.emit(Instruction::Mul); // AX = index * size
         }
         // If size is 1 (char), no scaling needed
     }
      /// Emits code to divide value in AX by sizeof type, for pointer difference scaling.
     fn emit_pointer_scaling_division(&mut self, target_type: DataType) {
          let size = target_type.size(); // Size in bytes
          if size > 1 {
             self.emit(Instruction::Push); // Push difference (in AX)
             self.emit_with_operand(Instruction::Imm, size as Value); // Load size in bytes
             self.emit(Instruction::Div); // AX = difference / size
         }
         // If size is 1 (char), no division needed
     }


     #[allow(dead_code)] // Keep this potentially unused helper for now
     fn peek_is_postfix_operator(&mut self) -> ParseResult<bool> {
        let result = matches!(
            self.peek_token()?, // Borrow contained here
            Some(Token::LBracket | Token::LParen | Token::Inc | Token::Dec)
        );
        Ok(result)
    }

     /// Peeks at the next token and returns its precedence if it's a binary operator
     /// with precedence >= min_precedence. Otherwise returns None.
     /// Takes &mut self because peek_token takes &mut self.
/// Peeks at the next token and returns its precedence if it's a binary operator
     /// with precedence >= min_precedence. Otherwise returns None.
     /// Takes &mut self because peek_token takes &mut self.
     fn peek_next_op_precedence(&mut self, min_precedence: i32) -> ParseResult<Option<i32>> {
        match self.tokens.peek() { // Mutable borrow on self.tokens starts here
            Some(Ok(token_info)) => {
                // Call the associated function (no `self` borrow needed)
                let precedence = Parser::get_operator_precedence(&token_info.token);
                if precedence >= min_precedence && precedence != PREC_BOTTOM {
                    Ok(Some(precedence)) // Return precedence if valid operator
                } else {
                    Ok(None) // Precedence too low or not an operator
                }
            }
            Some(Err(lex_err)) => Err(ParseError::LexerError(lex_err.clone())), // Handle lexer error
            None => Ok(None), // End of input
        }
        // Borrow from peek() ends here
    }


    /// Parses primary expressions: literals, identifiers (vars/calls), parentheses, unary ops.
    /// Handles subsequent postfix ops using an `if let` loop structure.
    fn parse_primary(&mut self) -> ParseResult<ExprInfo> {
        // --- Store token info before consuming (needed for function call Sys check) ---
        let primary_token_info = match self.tokens.peek() {
             Some(Ok(info)) => info.clone(),
             Some(Err(e)) => return Err(ParseError::LexerError(e.clone())),
             None => return Err(ParseError::EndOfInput),
        };
        let token_info = self.next_token()?; // Consume the token

        let mut expr_info: ExprInfo; // This will be updated by postfix ops

        match token_info.token { // Use the *consumed* token_info here
            // --- Cases for Number, StringLiteral, CharLiteral, Ident, LParen (Cast/Group), Unary Ops ---
            // (Keep the existing logic for all these primary/unary cases exactly as before)
            Token::Number(val) => {
                 self.emit_with_operand(Instruction::Imm, val as Value);
                 expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None };
            }
            Token::StringLiteral(s) => {
                 let addr = self.add_string_literal(&s);
                 self.emit_with_operand(Instruction::Imm, addr);
                 expr_info = ExprInfo { data_type: DataType::Ptr(Box::new(DataType::Char)), is_lvalue: false, lvalue_load_instr: None };
            }
            Token::CharLiteral(c) => {
                self.emit_with_operand(Instruction::Imm, c as Value);
                expr_info = ExprInfo { data_type: DataType::Char, is_lvalue: false, lvalue_load_instr: None };
            }
            Token::Ident(ref name) => {
                let sym = self.symbol_table.find(name)
                    .ok_or_else(|| ParseError::UndefinedSymbol(name.clone(), token_info.line, token_info.column))?
                    .clone();
                match sym.class {
                    SymbolClass::Num => { self.emit_with_operand(Instruction::Imm, sym.value as Value); expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None }; }
                    SymbolClass::Loc => { self.emit_with_operand(Instruction::Lea, sym.value as Value); let load_instr = Some(if sym.data_type == DataType::Char { Instruction::Lc } else { Instruction::Li }); expr_info = ExprInfo { data_type: sym.data_type.clone(), is_lvalue: true, lvalue_load_instr: load_instr }; }
                    SymbolClass::Glo => { self.emit_with_operand(Instruction::Imm, sym.value as Value); let load_instr = Some(if sym.data_type == DataType::Char { Instruction::Lc } else { Instruction::Li }); expr_info = ExprInfo { data_type: sym.data_type.clone(), is_lvalue: true, lvalue_load_instr: load_instr }; }
                    SymbolClass::Fun | SymbolClass::Sys => { self.emit_with_operand(Instruction::Imm, sym.value as Value); let fn_ptr_type = if sym.class == SymbolClass::Fun { DataType::Ptr(Box::new(sym.data_type)) } else { sym.data_type }; expr_info = ExprInfo { data_type: fn_ptr_type, is_lvalue: false, lvalue_load_instr: None }; }
                    SymbolClass::Enum => { return Err(ParseError::Other(format!("Cannot use enum tag '{}' directly in expression at {}:{}", name, token_info.line, token_info.column))); }
                    SymbolClass::Key => { return Err(ParseError::Other(format!("Unexpected keyword '{}' in expression at {}:{}", name, token_info.line, token_info.column))); }
                }
            }
            Token::LParen => { // Parentheses or Cast
                if matches!(self.peek_token()?, Some(Token::Int | Token::Char | Token::Void)) {
                    let is_cast = { /* Existing lookahead logic for cast */
                        let mut temp_lexer = self.tokens.clone(); let mut lookahead_count = 0; let max_lookahead = 4; let mut is_likely_cast = false;
                         if matches!(temp_lexer.next(), Some(Ok(TokenInfo{token: Token::Int | Token::Char | Token::Void, ..}))) {
                             lookahead_count += 1; while let Some(Ok(TokenInfo{token: Token::Asterisk, ..})) = temp_lexer.peek() { if lookahead_count >= max_lookahead { break; } temp_lexer.next(); lookahead_count += 1; }
                             if let Some(Ok(TokenInfo{token: Token::RParen, ..})) = temp_lexer.peek() { is_likely_cast = true; }
                         } is_likely_cast };
                    if is_cast { expr_info = self.parse_cast_expression()?; }
                    else { expr_info = self.parse_expression_with_level(PREC_BOTTOM)?; self.expect_token(Token::RParen, "')' to close parenthesis")?; }
                } else { expr_info = self.parse_expression_with_level(PREC_BOTTOM)?; self.expect_token(Token::RParen, "')' to close parenthesis")?; }
            }
            Token::Sizeof => { /* Existing Sizeof logic */
                 self.expect_token(Token::LParen, "'(' after sizeof")?;
                 let size = if matches!(self.peek_token()?, Some(Token::Int | Token::Char | Token::Void)) {
                     let is_type = { /* Existing lookahead logic for sizeof(type) */
                        let mut temp_lexer = self.tokens.clone(); let mut lookahead_count = 0; let max_lookahead = 4; let mut is_likely_type = false;
                         if matches!(temp_lexer.next(), Some(Ok(TokenInfo{token: Token::Int | Token::Char | Token::Void, ..}))) {
                             lookahead_count += 1; while let Some(Ok(TokenInfo{token: Token::Asterisk, ..})) = temp_lexer.peek() { if lookahead_count >= max_lookahead { break; } temp_lexer.next(); lookahead_count += 1; }
                              if let Some(Ok(TokenInfo{token: Token::RParen, ..})) = temp_lexer.peek() { is_likely_type = true; }
                         } is_likely_type };
                     if is_type { let target_type = self.parse_type()?; target_type.size() }
                     else { return Err(ParseError::InvalidSizeofTarget(token_info.line, token_info.column)); }
                 } else { return Err(ParseError::InvalidSizeofTarget(token_info.line, token_info.column)); };
                 self.expect_token(Token::RParen, "')' after sizeof argument")?;
                 self.emit_with_operand(Instruction::Imm, size as Value);
                 expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None }; }
            Token::Asterisk => { /* Existing Dereference (*) logic */
                let mut inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                 if inner_info.is_lvalue { if let Some(load_instr) = inner_info.lvalue_load_instr { self.emit(load_instr); inner_info.is_lvalue = false; inner_info.lvalue_load_instr = None; } else { return Err(ParseError::Other("Internal error: LValue operand for * missing load instruction".into())); } }
                 if let DataType::Ptr(target_type) = inner_info.data_type { let target_load_instr = Some(if *target_type == DataType::Char { Instruction::Lc } else { Instruction::Li }); expr_info = ExprInfo { data_type: *target_type, is_lvalue: true, lvalue_load_instr: target_load_instr }; }
                 else { return Err(ParseError::InvalidDereference(token_info.line, token_info.column)); } }
                 Token::BitAnd => { // *** CHANGED HERE from Token::Ampersand ***
                    // Parse operand expression; result -> AX (MUST be address for '&')
                    let inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                
                    // Operand MUST be an LValue. We DO NOT load the value.
                    if !inner_info.is_lvalue {
                        // Use the original token's info for error reporting
                        return Err(ParseError::InvalidAddressOf(token_info.line, token_info.column));
                    }
                    // AX holds the address. The result of '&' is that address, but it's an RValue.
                    // The type becomes Ptr(operand_type).
                    expr_info = ExprInfo {
                        data_type: DataType::Ptr(Box::new(inner_info.data_type)), // Create pointer type
                        is_lvalue: false, // The result of '&' itself is not an LValue
                        lvalue_load_instr: None // Not an LValue, so no load instruction
                    };
                }            Token::Not => { /* Existing Logical NOT (!) logic */
                let mut inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                if inner_info.is_lvalue { if let Some(load_instr) = inner_info.lvalue_load_instr { self.emit(load_instr); inner_info.is_lvalue = false; inner_info.lvalue_load_instr = None; } else { return Err(ParseError::Other("Internal error: LValue operand for ! missing load instruction".into())); } }
                self.emit_with_operand(Instruction::Imm, 0); self.emit(Instruction::Eq); expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None }; }
            Token::BitNot => { /* Existing Bitwise NOT (~) logic */
                let mut inner_info = self.parse_expression_with_level(PREC_UNARY)?;
                if inner_info.is_lvalue { if let Some(load_instr) = inner_info.lvalue_load_instr { self.emit(load_instr); inner_info.is_lvalue = false; inner_info.lvalue_load_instr = None; } else { return Err(ParseError::Other("Internal error: LValue operand for ~ missing load instruction".into())); } }
                self.emit_with_operand(Instruction::Imm, -1); self.emit(Instruction::Xor); expr_info = ExprInfo { data_type: DataType::Int, is_lvalue: false, lvalue_load_instr: None }; }
            Token::Add => { /* Existing Unary Plus (+) logic */
                expr_info = self.parse_expression_with_level(PREC_UNARY)?;
                if expr_info.is_lvalue { if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; expr_info.lvalue_load_instr = None; } else { return Err(ParseError::Other("Internal error: LValue operand for unary + missing load instruction".into())); } } }
            Token::Sub => { /* Existing Unary Minus (-) logic */
                expr_info = self.parse_expression_with_level(PREC_UNARY)?;
                if expr_info.is_lvalue { if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; expr_info.lvalue_load_instr = None; } else { return Err(ParseError::Other("Internal error: LValue operand for unary - missing load instruction".into())); } }
                if !expr_info.data_type.is_int_or_char() { return Err(ParseError::TypeMismatch("Cannot apply unary minus to non-arithmetic type".into(), token_info.line, token_info.column)); }
                self.emit(Instruction::Neg); expr_info.is_lvalue = false; }
                Token::Inc | Token::Dec => { // Prefix ++/-- logic
                    let op = token_info.token;
                    // Parse the operand *after* the ++/--
                    // This call NO LONGER auto-loads the LValue
                    let inner_info = self.parse_expression_with_level(PREC_UNARY)?;
    
                    // This check should now work correctly if the operand is an LValue
                    if !inner_info.is_lvalue {
                        return Err(ParseError::NotAnLValue("prefix ++/-- operand".into(), token_info.line, token_info.column));
                    }
    
                    // Ensure we have the load instruction associated with the LValue type.
                    let load_instr = inner_info.lvalue_load_instr.ok_or_else(|| ParseError::Other(format!(
                        "Internal error: LValue for prefix ++/-- at {}:{} missing load instruction", // Adjusted error message
                        token_info.line, token_info.column
                    )))?;
    
                    // inner_info.is_lvalue is TRUE, so AX holds the ADDRESS.
    
                    // Generate code for prefix ++/--:
                    self.emit(Instruction::Push); // 1. Push the address (from AX)
                    self.emit(load_instr);        // 2. Load the original value -> AX
                    self.emit(Instruction::Push); // 3. Push the original value (onto address)
                    self.emit_with_operand(Instruction::Imm, 1); // 4. Prepare amount (1) -> AX
                    if inner_info.data_type.is_pointer() { // Check if the LValue is a pointer type
                        // Pointer arithmetic: Need to scale the increment/decrement amount
                        let element_type = inner_info.data_type.deref().unwrap_or(DataType::Char); // Get pointed-to type
                        self.emit_pointer_scaling(element_type); // Scale amount in AX (1) by element size
                    }
                    // 5. Calculate new value: stack=[addr, orig_val], AX=scaled_1
                    self.emit(if op == Token::Inc { Instruction::Add } else { Instruction::Sub }); // AX = orig_val + scaled_1 = new_value (pops orig_val)
                    // 6. Store the new value (AX) back to the address (pops addr)
                    self.emit_store(&inner_info.data_type)?;
                    // 7. Result of prefix ++/-- is the NEW value, which is already in AX. Result is RValue.
                    expr_info = ExprInfo { data_type: inner_info.data_type, is_lvalue: false, lvalue_load_instr: None };
                }            // --- Error Cases ---
            Token::Eof => return Err(ParseError::UnexpectedToken { expected: "primary expression".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
            _ => return Err(ParseError::UnexpectedToken { expected: "primary expression".into(), found: token_info.token, line: token_info.line, column: token_info.column }),
        }

        // --- Handle Postfix Operators (Using `if let` loop structure) ---
        loop {
            // Peek using `if let`. The borrow for peek is confined here.
            if let Some(Ok(peeked_info)) = self.tokens.peek() {
                // Clone the token to release the borrow from peek immediately.
                let cloned_token = peeked_info.token.clone();

                // Decide based on the *cloned* token. No borrow on self.tokens here.
                match cloned_token {
                    Token::LBracket | Token::LParen | Token::Inc | Token::Dec => {
                        // It IS a postfix operator. Now it's safe to consume it.
                        // This takes a fresh mutable borrow.
                        let postfix_token_info = self.next_token()?;

                        // --- Handle the specific postfix operator ---
                        // (Use the *consumed* postfix_token_info)
                        match postfix_token_info.token {
                             Token::LBracket => { // Array indexing a[i]
                                 // --- Array Indexing Logic --- (Same as before)
                                 if expr_info.is_lvalue { if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; expr_info.lvalue_load_instr = None; } else { let (l,c) = (postfix_token_info.line, postfix_token_info.column); return Err(ParseError::Other(format!("Internal error: LValue base for [] missing load instruction at {}:{}", l, c))); } }
                                 if !expr_info.data_type.is_pointer() { let (l,c) = (postfix_token_info.line, postfix_token_info.column); return Err(ParseError::TypeMismatch("Attempting to index non-pointer type".into(), l, c)); }
                                 let element_type = expr_info.data_type.deref().unwrap_or(DataType::Char);
                                 self.emit(Instruction::Push); // Push base ptr value
                                 let mut index_info = self.parse_expression_with_level(PREC_BOTTOM)?;
                                 if index_info.is_lvalue { if let Some(load_instr) = index_info.lvalue_load_instr { self.emit(load_instr); index_info.is_lvalue = false; index_info.lvalue_load_instr = None; } else { let (l, c) = self.current_pos(); return Err(ParseError::Other(format!("Internal error: LValue index for [] missing load instruction near {}:{}", l, c))); } }
                                 if !index_info.data_type.is_int_or_char() { let (l, c) = self.current_pos(); return Err(ParseError::TypeMismatch("Array index must be integer".into(), l, c)); }
                                 self.expect_token(Token::RBracket, "']' to close array index")?;
                                 self.emit_pointer_scaling(element_type.clone()); // Scale index in AX
                                 self.emit(Instruction::Add); // AX = base_ptr + scaled_index (pops base_ptr)
                                 let target_load_instr = Some(if element_type == DataType::Char { Instruction::Lc } else { Instruction::Li });
                                 // Result is LValue (address)
                                 expr_info = ExprInfo { data_type: element_type, is_lvalue: true, lvalue_load_instr: target_load_instr };
                             }
                             Token::LParen => { // Function call f(...)
                                 // --- Function Call Logic --- (Same as before)
                                 if expr_info.is_lvalue { if let Some(load_instr) = expr_info.lvalue_load_instr { self.emit(load_instr); expr_info.is_lvalue = false; expr_info.lvalue_load_instr = None; } else { let (l,c) = (postfix_token_info.line, postfix_token_info.column); return Err(ParseError::Other(format!("Internal error: LValue function target missing load instruction at {}:{}", l, c))); } }
                                 // AX holds func address or syscall id
                                 let func_target_type_info = expr_info.data_type.clone(); // Type info before call
                                 let mut is_syscall = false; let mut syscall_instr: Option<Instruction> = None;

                                 // Use original primary token info (passed implicitly via closure capture or explicit arg if refactored)
                                 if let Token::Ident(ref name) = primary_token_info.token {
                                     if let Some(sym) = self.symbol_table.find(name) {
                                         if sym.class == SymbolClass::Sys {
                                             is_syscall = true; syscall_instr = Instruction::try_from(sym.value as Value).ok();
                                             if syscall_instr.is_none() { return Err(ParseError::Other(format!("Invalid opcode {} for syscall '{}' at {}:{}", sym.value, name, primary_token_info.line, primary_token_info.column))); }
                                         }
                                     }
                                 }
                                 if !is_syscall { self.emit(Instruction::Push); } // Push func address if regular call
                                 // LParen already consumed

                                 // Determine return type based on whether it's syscall or regular call
                                 let return_type = if is_syscall {
                                      // Syscall 'type' in symbol table is the return type
                                      func_target_type_info
                                  } else {
                                      // Regular function: type was Ptr(ReturnType)
                                      func_target_type_info.deref().unwrap_or(DataType::Int) // Default to int if deref fails? Error?
                                  };

                                 let (return_expr_info, arg_count) = self.parse_function_call_args(return_type)?;
                                 if let Some(instr) = syscall_instr { self.emit(instr); }
                                 else {
                                      self.emit(Instruction::Call); // Pops address
                                      if arg_count > 0 { self.emit_with_operand(Instruction::Adj, arg_count as Value); } // Adjust stack for args
                                 }
                                 expr_info = return_expr_info; // Update expr_info with result
                             }
                             Token::Inc | Token::Dec => { // Postfix ++/--
                                 // --- Postfix ++/-- Logic --- (Same as before)
                                 let op_token = postfix_token_info.token;
                                 if !expr_info.is_lvalue { let (l,c) = (postfix_token_info.line, postfix_token_info.column); return Err(ParseError::NotAnLValue("postfix ++/-- operand".into(), l, c)); }
                                 let load_instr = expr_info.lvalue_load_instr.ok_or_else(|| { let (l,c) = (postfix_token_info.line, postfix_token_info.column); ParseError::Other(format!("Internal error: postfix ++/-- operand missing load instruction at {}:{}", l, c))})?;
                                 let data_type = expr_info.data_type.clone();

                                 // AX has address
                                 self.emit(Instruction::Push); // [S: addr]
                                 self.emit(load_instr);        // AX = orig_val
                                 self.emit(Instruction::Push); // [S: addr, orig_val]
                                 self.emit(Instruction::Push); // [S: addr, orig_val, orig_val] (Push value to save it)
                                 self.emit_with_operand(Instruction::Imm, 1); // AX = 1
                                 if data_type.is_pointer() { let element_type = data_type.deref().unwrap_or(DataType::Char); self.emit_pointer_scaling(element_type); } // AX = scaled_1
                                 // Pop one orig_val, calc new_val
                                 self.emit(if op_token == Token::Inc { Instruction::Add } else { Instruction::Sub }); // AX = orig_val + scaled_1 = new_val. [S: addr, orig_val]
                                 self.emit_store(&data_type)?; // Pops addr, stores AX(new_val). [S: orig_val]
                                 // Result of postfix is original value. Pop it into AX.
                                 self.emit_with_operand(Instruction::Imm, 0); // AX = 0
                                 self.emit(Instruction::Add); // AX = 0 + orig_val (pops orig_val). [S: ], AX = orig_val
                                 // Result is RValue (original value)
                                 expr_info = ExprInfo { data_type, is_lvalue: false, lvalue_load_instr: None };
                             }
                            _ => unreachable!("Inner match should only receive postfix ops due to outer match"),
                        }
                         // Successfully processed a postfix op, continue loop
                    }
                    _ => {
                        // The cloned token was not a postfix operator
                        break; // Exit the loop
                    }
                }
            } else if let Some(Err(e)) = self.tokens.peek() {
                // Peek resulted in a lexer error
                return Err(ParseError::LexerError(e.clone()));
            } else {
                // Peek resulted in None (End Of Input)
                break; // Exit the loop
            }
        } // End postfix loop

        // Final LValue load check happens in parse_expression_with_level after this returns

        Ok(expr_info)
    }


    // --- Other methods like parse_function_call_args, parse_type, parse_cast_expression, map_add_error ---
    // --- remain the same as in the previous working versions ---

    /// Parses function call arguments and emits PUSH instructions.
    /// CALL/Syscall/ADJ are handled by the caller.
    /// Returns the ExprInfo for the function's return value and the number of args pushed.
    fn parse_function_call_args(&mut self, func_return_type: DataType) -> ParseResult<(ExprInfo, i64)> {
        // (Keep existing logic for parsing args L->R, storing code, emitting PUSH R->L)
        let return_type = func_return_type;
        let mut arg_count: i64 = 0;
        let mut arg_codes: Vec<Vec<Value>> = Vec::new();

        if self.peek_token()? != Some(&Token::RParen) {
            loop {
                 let mut temp_code_buffer: Vec<Value> = Vec::new();
                 let original_code = mem::take(&mut self.code);
                 mem::swap(&mut self.code, &mut temp_code_buffer); // Use temp buffer

                 let mut arg_info = self.parse_expression_with_level(PREC_ASSIGN)?; // Parse arg -> temp buffer
                 if arg_info.is_lvalue { // Load arg value if it's an LValue
                     if let Some(load_instr) = arg_info.lvalue_load_instr {
                         self.emit(load_instr); // Emit load into temp buffer
                         arg_info.is_lvalue = false; arg_info.lvalue_load_instr = None;
                     } else {
                        mem::swap(&mut self.code, &mut temp_code_buffer); // Restore before error
                        self.code = original_code;
                        return Err(ParseError::Other("Internal error: LValue argument missing load instruction".into()));
                     }
                 }

                 mem::swap(&mut self.code, &mut temp_code_buffer); // Swap back
                 self.code = original_code; // Restore original

                 arg_codes.push(temp_code_buffer);
                 arg_count += 1;

                 if !self.consume_optional(Token::Comma)? { break; }
            }
        }
        self.expect_token(Token::RParen, "')' after function arguments")?;

        // Emit argument code and PUSH instructions in REVERSE order
        for arg_code in arg_codes.iter().rev() {
            self.code.extend_from_slice(arg_code); // Emit arg calculation -> AX
            self.emit(Instruction::Push);          // Push AX
        }

        Ok((
            ExprInfo { data_type: return_type, is_lvalue: false, lvalue_load_instr: None },
            arg_count
        ))
    }

    /// Parses a type specifier (int, char, void, pointers) used in casts or sizeof.
    fn parse_type(&mut self) -> ParseResult<DataType> {
        // (Keep existing logic)
        let base_type = match self.peek_token()? {
            Some(Token::Int) => { self.next_token()?; DataType::Int },
            Some(Token::Char) => { self.next_token()?; DataType::Char },
            Some(Token::Void) => { self.next_token()?; DataType::Void },
            _ => { let ti = self.next_token()?; return Err(ParseError::UnexpectedToken{expected:"type name (int, char, void)".into(), found:ti.token, line:ti.line, column:ti.column})}
        };
        self.parse_pointers(base_type)
    }

    /// Parses a type cast expression `(type) expr`
    fn parse_cast_expression(&mut self) -> ParseResult<ExprInfo> {
        // (Keep existing logic - assumes LParen was consumed by caller)
        let target_type = self.parse_type()?;
        self.expect_token(Token::RParen, "')' after type cast")?;
        let mut inner_expr_info = self.parse_expression_with_level(PREC_UNARY)?;
        if inner_expr_info.is_lvalue { // Load value if needed
            if let Some(load_instr) = inner_expr_info.lvalue_load_instr {
                 self.emit(load_instr); inner_expr_info.is_lvalue = false; inner_expr_info.lvalue_load_instr = None;
            } else {
                 return Err(ParseError::Other("Internal error: LValue operand for cast missing load instruction".into()));
            }
        }
        // Casting itself doesn't generate code, just updates type info. Result is RValue.
        Ok(ExprInfo { data_type: target_type, is_lvalue: false, lvalue_load_instr: None })
    }

    /// Helper to map symbol table errors to ParseError::Redeclaration or Other.
    fn map_add_error(symbol_error: String, name: &str, line: usize, column: usize) -> ParseError {
        // (Keep existing logic)
        if symbol_error.contains("already defined") || symbol_error.contains("Redeclaration") {
            ParseError::Redeclaration(name.to_string(), line, column)
        } else {
            ParseError::Other(format!("Symbol table error for '{}' at {}:{}: {}", name, line, column, symbol_error))
        }
     }

} // impl Parser

// --- Add helper extension trait for DataType ---
// This should ideally be in symbol.rs or shared, but ensure parser.rs can see it.
// Make sure WORD_SIZE here matches the VM's VALUE_SIZE_BYTES if you redefine it.

trait DataTypeExt {
    fn is_pointer(&self) -> bool;
    fn is_int_or_char(&self) -> bool;
    fn size(&self) -> i64; // Get size in bytes
    fn alignment(&self) -> usize; // Get alignment requirement in bytes
    fn size_in_words(&self) -> i64; // Get size in VM words (for offsets/ENT)
    fn deref(&self) -> Option<DataType>; // Get type pointed to
}

impl DataTypeExt for DataType {
     #[inline] fn is_pointer(&self) -> bool { matches!(self, DataType::Ptr(_)) }
     #[inline] fn is_int_or_char(&self) -> bool { matches!(self, DataType::Int | DataType::Char) }
     // Use WORD_SIZE consistently for Int/Ptr size
     #[inline] fn size(&self) -> i64 { match self { DataType::Char => 1, DataType::Int | DataType::Ptr(_) => WORD_SIZE as i64, DataType::Void => 0 } }
     #[inline] fn alignment(&self) -> usize { match self { DataType::Char => 1, DataType::Int | DataType::Ptr(_) => WORD_SIZE, DataType::Void => 1 } }
     // Assuming VM treats char/int/ptr as taking 1 word slot on stack/locals
     #[inline] fn size_in_words(&self) -> i64 { match self { DataType::Char | DataType::Int | DataType::Ptr(_) => 1, DataType::Void => 0 } }
     #[inline] fn deref(&self) -> Option<DataType> { match self { DataType::Ptr(inner) => Some(*inner.clone()), _ => None } }
}
