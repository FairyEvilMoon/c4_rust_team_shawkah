//! Parser for the C4 language subset.
extern crate lexer;
use crate::lexer::{Lexer, Token, LexerError};
use crate::vm::Instruction;
use crate::symbol::{SymbolTable, SymbolEntry, SymbolClass, DataType};
use std::iter::Peekable;
use std::mem; // For mem::discriminant
extern crate libc;
use libc::*;

// Operator Precedence Levels (Matching C4's enum order)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    P0_Lowest = 0, // Sentinel
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
        _ => Precedence::P0_Lowest,
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
    lexer: Peekable<Lexer<'a>>,
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

    // --- Token Handling ---
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

    // --- Code Emission Helpers ---
    
    // Emit a single instruction
    #[inline]
    fn emit(&mut self, instruction: Instruction) {
        self.code.push(instruction as i64);
    }

    // Emit an instruction opcode followed by its operand
    #[inline]
    fn emit_operand(&mut self, instruction: Instruction, operand: i64) {
        self.code.push(instruction as i64);
        self.code.push(operand);
    }

    // Get the next available code address
    #[inline]
    fn next_code_addr(&self) -> usize {
        self.code.len()
    }

    // Patch a previously emitted instruction operand (e.g., for jumps)
    #[inline]
    fn patch_jump(&mut self, jump_instruction_addr: usize, target_addr: usize) -> Result<(), ParserError> {
        // Check bounds more carefully
        if jump_instruction_addr >= self.code.len() || jump_instruction_addr.checked_add(1).is_none() || jump_instruction_addr + 1 >= self.code.len() {
             // Use return Err to fix type inference issue E0282
             return Err(ParserError::InternalError(format!(
                 "Jump address {} out of bounds for patching (code len {})",
                 jump_instruction_addr, self.code.len()
             )));
        }
        self.code[jump_instruction_addr + 1] = target_addr as i64;
        Ok(())
    }

    // --- Symbol Table Management ---
    fn add_global_data(&mut self, bytes: &[u8]) -> usize {
        let addr = self.data_segment.len();
        self.data_segment.extend_from_slice(bytes);
        // Align data segment to word boundary
        while self.data_segment.len() % mem::size_of::<i64>() != 0 {
            self.data_segment.push(0);
        }
        addr
    }

    // --- Parsing Functions ---

    // Parse the entire input source code
    pub fn parse(&mut self) -> Result<(Vec<i64>, Option<usize>, Vec<u8>), ParserError> {
        self.add_builtins()?; // Add built-in functions (printf, exit, etc.) to the symbol table
        while self.current_token != Token::EOF {
            self.parse_global_declaration()?;
        }
        
        if self.main_entry_point.is_none() {
            return Err(ParserError::UndefinedSymbol("main() function not defined".to_string()));
        }
        Ok((self.code.clone(), self.main_entry_point, self.data_segment.clone()))
    }

    fn add_builtins(&mut self) -> Result<(), ParserError> {
        // Add keywords first
        let keywords = [
            ("char", Token::Char), ("else", Token::Else), ("enum", Token::Enum),
            ("if", Token::If), ("int", Token::Int), ("return", Token::Return),
            ("sizeof", Token::Sizeof), ("while", Token::While),
        ];
        let builtins = [
            ("open", Instruction::OPEN), ("read", Instruction::READ), ("close", Instruction::CLOS),
            ("printf", Instruction::PRTF), ("malloc", Instruction::MALC), ("free", Instruction::FREE),
            ("memset", Instruction::MSET), ("memcmp", Instruction::MCMP), ("exit", Instruction::EXIT),
        ];
        for (name, instr) in builtins.iter() {
            self.symbols.add(
                name.to_string(),
                Token::Ident(name.to_string()),
                SymbolClass::Sys,
                DataType::Int,
                *instr as i64,
            )?;
        }

        for (name, token) in keywords.iter() {
            self.symbols.add(
                name.to_string(),
                token.clone(),
                SymbolClass::Key,
                DataType::Void,
                0,
            )?;
        }

        Ok(())
    }

    // Parse a global declaration
    fn parse_global_declaration(&mut self) -> Result<(), ParserError> {
        let base_type = match self.current_token {
            Token::Enum => return self.parse_enum_declaration(), // Enums handled separately
            Token::Int => { self.consume()?; DataType::Int },
            Token::Char => { self.consume()?; DataType::Char },
            // Assume int if no type specifier (implicit int - C style, C4 allows this)
            Token::Asterisk | Token::Ident(_) => DataType::Int,
            _ => return Err(ParserError::UnexpectedToken(self.current_token.clone(), "type specifier or identifier".to_string())),
        };

        // Parse potential pointers and the identifier name
        let (mut current_type, ident_name) = self.parse_type_and_identifier(base_type)?;

        // Function definition or global variable?
        if self.check(&Token::LParen) {
            // Function definition
            self.parse_function_definition(ident_name, current_type)?;
        } else {
            // Global variable declaration list
             loop {
                 // Define the global variable
                 let data_addr = self.data_segment.len() as i64;
                  // Allocate space in data segment based on type size
                  let size = current_type.size_of() as usize;
                  self.data_segment.resize(self.data_segment.len() + size, 0);
                   // Align data segment to word boundary
                  while self.data_segment.len() % mem::size_of::<i64>() != 0 {
                    self.data_segment.push(0);
                  }


                 self.symbols.add(
                     ident_name.clone(),
                     Token::Ident(ident_name.clone()), // Store the original token type
                     SymbolClass::Glo,
                     current_type.clone(),
                     data_addr, // Value is the address in the data segment
                 ).map_err(|e| ParserError::Redefinition(e))?;


                 // Check for more declarations in the same line
                 if self.check(&Token::Comma) {
                      self.consume()?; // Consume comma
                      // Parse next variable in the list (re-uses base_type)
                      let (next_type, next_name) = self.parse_type_and_identifier(base_type)?;
                      current_type = next_type;
                      ident_name = next_name;
                 } else {
                     break;
                 }
             }

            self.expect(Token::Semicolon)?;
        }

        Ok(())
    }

    // Parse type specifiers and pointer declaration
    fn parse_type_and_identifier(&mut self, mut base_type: DataType) -> Result<(DataType, String), ParserError> {
        while self.check(&Token::Asterisk) {
            self.consume()?;
            base_type = DataType::pointer_to(base_type);
        }

        if let Token::Ident(name) = self.current_token.clone() {
            self.consume()?;
            Ok((base_type, name))
        } else {
            Err(ParserError::UnexpectedToken(self.current_token.clone(), "identifier".to_string()))
        }
    }

    // Parse an enum declaration
    fn parse_enum_declaration(&mut self) -> Result<(), ParserError> {
        self.expect(Token::Enum)?;
        let enum_name = if let Token::Ident(name) = self.current_token.clone() {
            self.consume()?;
            Some(name)
        } else {
            None // Allow anonymous enums
        };

        if !self.check(&Token::LBrace) {
           // Enum forward declaration or usage - C4 doesn't really support this way
           // Assume definition follows if name is present
            if enum_name.is_some() && self.check(&Token::Semicolon) {
                self.consume()?;
                // Warn or error - C4 doesn't use enum tags like this
                return Err(ParserError::NotImplemented("Enum forward declaration/tags not fully supported like C4".to_string()));
            }
            return Err(ParserError::UnexpectedToken(self.current_token.clone(), "{".to_string()));
        }
        self.expect(Token::LBrace)?;

        let mut current_value: i64 = 0;
        loop {
            if self.check(&Token::RBrace) { break; }

            let member_name = if let Token::Ident(name) = self.current_token.clone() {
                self.consume()?;
                name
            } else {
                return Err(ParserError::UnexpectedToken(self.current_token.clone(), "enum member identifier".to_string()));
            };

            if self.check(&Token::Assign) {
                self.consume()?;
                if let Token::Number(val) = self.current_token {
                    current_value = val;
                    self.consume()?;
                } else {
                    return Err(ParserError::UnexpectedToken(self.current_token.clone(), "number for enum value".to_string()));
                }
            }

            // Add enum member to symbol table as a number constant (like C4)
             self.symbols.add(
                 member_name.clone(),
                 Token::Ident(member_name),
                 SymbolClass::Num,
                 DataType::Int, // Enums are ints
                 current_value,
             ).map_err(|e| ParserError::Redefinition(e))?;

            current_value += 1; // Increment for next default value

            if self.check(&Token::RBrace) { break; }
            self.expect(Token::Comma)?;
        }
        self.expect(Token::RBrace)?;
        self.expect(Token::Semicolon)?; // Global enums usually end with ;

        Ok(())
   }

   // Parse a function definition
   fn parse_function_definition(&mut self, name: String, return_type: DataType) -> Result<(), ParserError> {
        // Check for redefinition before adding
        if self.symbols.find(&name).is_some() {
            // C4 allows forward declarations implicitly, but not redefinitions
            return Err(ParserError::Redefinition(format!("Symbol '{}' already defined", name)));
        }

        // Add function symbol *before* parsing body to allow recursion
        let entry_point = self.next_code_addr();
        self.symbols.add(
            name.clone(),
            Token::Ident(name.clone()),
            SymbolClass::Fun,
            return_type.clone(),
            entry_point as i64,
        ).map_err(|e| ParserError::Redefinition(e))?;


        if name == "main" {
            self.main_entry_point = Some(entry_point);
        }

        self.expect(Token::LParen)?;

        // Enter function scope
        self.symbols.enter_scope();
        let mut param_count = 0;
        self.local_offset = 0; // Reset local offset (params are positive offsets, locals negative)

        // Parse parameters
        while !self.check(&Token::RParen) {
            let param_base_type = match self.current_token {
                Token::Int => { self.consume()?; DataType::Int },
                Token::Char => { self.consume()?; DataType::Char },
                _ => return Err(ParserError::UnexpectedToken(self.current_token.clone(), "parameter type".to_string())),
            };
            let (param_type, param_name) = self.parse_type_and_identifier(param_base_type)?;

            self.symbols.add(
                param_name.clone(),
                Token::Ident(param_name),
                SymbolClass::Loc,
                param_type,
                param_count + 1, // C4 style: Param 1 is offset 1, Param 2 is offset 2 etc. from frame base
            ).map_err(|e| ParserError::Redefinition(e))?;
            param_count += 1;

            if !self.check(&Token::RParen) {
                self.expect(Token::Comma)?;
            }
        }
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;


        self.local_offset = 0; // Start local allocation index
        let mut local_var_count = 0;


        // Parse local declarations
        while self.check(&Token::Int) || self.check(&Token::Char) {
            let base_type = if self.check(&Token::Int) { self.consume()?; DataType::Int } else { self.consume()?; DataType::Char };
            loop { // Handle multiple declarations per line (int a, *b;)
                let (var_type, var_name) = self.parse_type_and_identifier(base_type)?;
                local_var_count += 1;
                self.local_offset += 1; // Increment index *before* using it

                // Add local variable symbol
                self.symbols.add(
                    var_name.clone(),
                    Token::Ident(var_name),
                    SymbolClass::Loc,
                    var_type,
                    -(self.local_offset as i64), // Locals have negative offsets from BP
                ).map_err(|e| ParserError::Redefinition(e))?;

                if !self.check(&Token::Comma) { break; }
                self.consume()?; // Consume comma
            }
            self.expect(Token::Semicolon)?;
        }

        // Emit function entry code
        self.emit_operand(Instruction::ENT, local_var_count as i64); // ENT takes number of local slots

        // Parse function body (statements)
        while !self.check(&Token::RBrace) {
            self.parse_statement()?;
        }
        self.expect(Token::RBrace)?;

        // Emit function leave code (implicit return if no explicit LEV emitted)
        // Check if last emitted instruction was LEV. If not, add one.
        if self.code.last() != Some(&(Instruction::LEV as i64)) {
            self.emit(Instruction::LEV);
        }

        // Leave function scope (restores shadowed symbols)
        self.symbols.leave_scope();

        Ok(())
    }

    // Parse a statement
    fn parse_statement(&mut self) -> Result<(), ParserError> {
        match self.current_token {
            Token::If => self.parse_if_statement(),
            Token::While => self.parse_while_statement(),
            Token::Return => self.parse_return_statement(),
            Token::LBrace => self.parse_block_statement(),
            Token::Semicolon => { // Empty statement
                self.consume()?;
                Ok(())
            }
            _ => self.parse_expression_statement(), // Default to expression statement
        }
    }

    // Parse an if statement
    fn parse_if_statement(&mut self) -> Result<(), ParserError> {
        self.expect(Token::If)?;
        self.expect(Token::LParen)?;
        self.parse_expression(Precedence::Assign)?; // Parse condition
        self.expect(Token::RParen)?;

        // Emit Branch if Zero (BZ) - jump if condition is false
        self.emit(Instruction::BZ);
        let jump_if_false_addr = self.next_code_addr(); // Address of the operand for BZ
        self.emit_operand(Instruction::IMM, 0); // Placeholder jump target

        // Parse 'then' block
        self.parse_statement()?;

        let jump_past_else_addr = if self.check(&Token::Else) {
             self.consume()?; // Consume 'else'

             // Emit JMP to skip the 'else' block if 'then' block executed
             self.emit(Instruction::JMP);
             let addr = self.next_code_addr();
             self.emit_operand(Instruction::IMM, 0); // Placeholder jump target

             // Patch the BZ instruction to jump here (start of else block)
             let else_start_addr = self.next_code_addr();
             self.patch_jump(jump_if_false_addr -1, else_start_addr)?; // BZ instr addr is jump_addr - 1

             // Parse 'else' block
             self.parse_statement()?;
             Some(addr) // Return address of the JMP operand past the else
        } else {
            None // No else block
        };

        // Patch jumps
        let end_if_addr = self.next_code_addr();
        if let Some(jmp_addr) = jump_past_else_addr {
             self.patch_jump(jmp_addr - 1, end_if_addr)?; // Patch JMP past else
        } else {
             // If no else, patch the initial BZ to jump here
            self.patch_jump(jump_if_false_addr - 1, end_if_addr)?;
        }

        Ok(())
    }

    // Parse a while statement
    fn parse_while_statement(&mut self) -> Result<(), ParserError> {
        self.expect(Token::While)?;
        let loop_start_addr = self.next_code_addr(); // Address to jump back to

        self.expect(Token::LParen)?;
        self.parse_expression(Precedence::Assign)?; // Parse condition
        self.expect(Token::RParen)?;

        // Emit Branch if Zero (BZ) - jump out of loop if condition is false
        self.emit(Instruction::BZ);
        let jump_out_addr = self.next_code_addr(); // Address of the operand for BZ
        self.emit_operand(Instruction::IMM, 0); // Placeholder jump target

        // Parse loop body
        self.parse_statement()?;

        // Emit JMP back to the start of the loop condition
        self.emit_operand(Instruction::JMP, loop_start_addr as i64);

        // Patch the BZ jump to the instruction after the loop
        let loop_end_addr = self.next_code_addr();
        self.patch_jump(jump_out_addr - 1, loop_end_addr)?;

        Ok(())
    }

    // Parse a return statement
    fn parse_return_statement(&mut self) -> Result<(), ParserError> {
        self.expect(Token::Return)?;
        if !self.check(&Token::Semicolon) {
            self.parse_expression(Precedence::Assign)?; // Parse return value expression (result in ax)
            true
        } else {
             false
        };
        self.expect(Token::Semicolon)?;
        self.emit(Instruction::LEV); // Emit leave instruction
        Ok(())
    }

    // Parse a block statement {...}
    fn parse_block_statement(&mut self) -> Result<(), ParserError> {
        self.expect(Token::LBrace)?;
        // Note: Scoping for locals is handled by function entry/exit.
        // C block scope for variables is not handled by C4 symbol table logic.
        // All locals are function-scoped.

        while !self.check(&Token::RBrace) {
             // C4 doesn't parse declarations inside blocks, only at function top.
             // So we only expect statements here.
             if self.check(&Token::Int) || self.check(&Token::Char) || self.check(&Token::Enum){
                  return Err(ParserError::SyntaxError("Variable declarations are only allowed at the top of a function in C4".to_string()));
             } else {
                self.parse_statement()?;
             }
        }
        self.expect(Token::RBrace)?;
        Ok(())
    }

    // Parse an expression statement (assignment or function called followed by ;)
    fn parse_expression_statement(&mut self) -> Result<(), ParserError> {
        self.parse_expression(Precedence::Assign)?;
        self.expect(Token::Semicolon)?;
        Ok(())
    }

    // Parse an expression using precedence climbing
    fn parse_expression(&mut self, min_precedence: Precedence) -> Result<DataType, ParserError> {
        // Parse the left-hand side (primary, unary, or nested expression)
        let mut left_type = self.parse_unary_or_primary()?;

        // Precedence climbing loop
        loop {
            let current_prec = token_precedence(&self.current_token);
            if current_prec < min_precedence { // Using `<` because higher precedence binds tighter
                break;
            }

            // Handle binary operators, postfix operators, conditional
            let token = self.current_token.clone(); // Clone token for use after consume

             match token {
                 // --- Assignment ---
                 Token::Assign => {
                    // Ensure LHS is an lvalue (requires address)
                    // Check the last emitted instruction sequence. LI/LC means we have value, not address.
                    // LEA or IMM (for globals) means we have address.
                    let last_op = self.code.last().map(|&op| Instruction::from(op));
                     match last_op {
                         Some(Instruction::LI) | Some(Instruction::LC) => {
                             // Convert load to push address
                             self.code.pop(); // Remove LI/LC
                             self.emit(Instruction::PSH); // Push address left by LEA/IMM
                         }
                         _ => return Err(ParserError::TypeError("Lvalue required for assignment".to_string())),
                     }

                    self.consume()?; // Consume '='
                    let right_type = self.parse_expression(Precedence::Assign)?; // Right associative for assign

                    // Emit store instruction based on LHS type
                    match left_type {
                        DataType::Char => self.emit(Instruction::SC),
                        DataType::Int | DataType::Ptr(_) => self.emit(Instruction::SI),
                        DataType::Void => return Err(ParserError::TypeError("Cannot assign to void".to_string())),
                    }
                    // Assignment result is the assigned value (already in ax from right expr)
                    // Type remains the type of the LHS
                 }

                 // --- Arithmetic / Bitwise / Logical ---
                 Token::Add => { self.emit(Instruction::PSH); self.consume()?; let right = self.parse_expression(Precedence::Add)?; self.emit_binary_op(Instruction::ADD, left_type, right)?; left_type = DataType::Int; } // Pointer arith handled in emit
                 Token::Sub => { self.emit(Instruction::PSH); self.consume()?; let right = self.parse_expression(Precedence::Add)?; self.emit_binary_op(Instruction::SUB, left_type, right)?; left_type = DataType::Int; } // Pointer arith handled in emit
                 Token::Mul => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Mul)?; self.emit(Instruction::MUL); left_type = DataType::Int; }
                 Token::Div => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Mul)?; self.emit(Instruction::DIV); left_type = DataType::Int; }
                 Token::Mod => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Mul)?; self.emit(Instruction::MOD); left_type = DataType::Int; }
                 Token::BitOr => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::BitOr)?; self.emit(Instruction::OR); left_type = DataType::Int; }
                 Token::BitXor=> { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::BitXor)?; self.emit(Instruction::XOR); left_type = DataType::Int; }
                 Token::Ampersand | Token::BitAnd => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::BitAnd)?; self.emit(Instruction::AND); left_type = DataType::Int; }
                 Token::Eq => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Eq)?; self.emit(Instruction::EQ); left_type = DataType::Int; }
                 Token::Ne => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Eq)?; self.emit(Instruction::NE); left_type = DataType::Int; }
                 Token::Lt => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Lt)?; self.emit(Instruction::LT); left_type = DataType::Int; }
                 Token::Gt => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Lt)?; self.emit(Instruction::GT); left_type = DataType::Int; }
                 Token::Le => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Lt)?; self.emit(Instruction::LE); left_type = DataType::Int; }
                 Token::Ge => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Lt)?; self.emit(Instruction::GE); left_type = DataType::Int; }
                 Token::Shl => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Shl)?; self.emit(Instruction::SHL); left_type = DataType::Int; }
                 Token::Shr => { self.emit(Instruction::PSH); self.consume()?; self.parse_expression(Precedence::Shl)?; self.emit(Instruction::SHR); left_type = DataType::Int; }

                 // --- Logical (Short-circuiting) ---
                 Token::LogOr => { // a || b --> if a is true (non-zero), result is 1, else eval b
                     self.emit(Instruction::BNZ); // Branch if non-zero (true)
                     let jump_if_true_addr = self.next_code_addr();
                     self.emit_operand(Instruction::IMM, 0); // Placeholder
                     self.consume()?;
                     self.parse_expression(Precedence::LogAnd)?; // Evaluate right side only if needed
                     let end_addr = self.next_code_addr();
                     self.patch_jump(jump_if_true_addr - 1, end_addr)?;
                     // If we got here, left was false, result is bool of right side (ax!=0)
                     // C4 doesn't explicitly boolify, result is ax. Let's keep ax.
                     left_type = DataType::Int;
                 }
                 Token::LogAnd => { // a && b --> if a is false (zero), result is 0, else eval b
                     self.emit(Instruction::BZ); // Branch if zero (false)
                     let jump_if_false_addr = self.next_code_addr();
                     self.emit_operand(Instruction::IMM, 0); // Placeholder
                     self.consume()?;
                     self.parse_expression(Precedence::BitOr)?; // Evaluate right side only if needed
                     let end_addr = self.next_code_addr();
                     self.patch_jump(jump_if_false_addr - 1, end_addr)?;
                     // If we got here, left was true, result is bool of right side (ax!=0)
                     // C4 doesn't explicitly boolify, result is ax. Let's keep ax.
                     left_type = DataType::Int;
                 }

                 // --- Postfix Operators ---
                 Token::LBracket => { // Array indexing: ptr[index]
                    let ptr_type = left_type;
                     if ptr_type.pointer_level() == 0 {
                        return Err(ParserError::TypeError("Attempting to index a non-pointer type".to_string()));
                     }
                    self.emit(Instruction::PSH); // Push pointer value
                    self.consume()?; // Consume '['
                    let index_type = self.parse_expression(Precedence::Assign)?; // Parse index expression
                    self.expect(Token::RBracket)?; // Consume ']'

                    // Emit pointer arithmetic: addr = base_ptr + index * sizeof(*base_ptr)
                    let element_type = ptr_type.deref().unwrap(); // We checked it's a pointer
                    if element_type.size_of() > 1 {
                        self.emit_operand(Instruction::IMM, element_type.size_of());
                        self.emit(Instruction::MUL); // index *= sizeof(element)
                    }
                    self.emit(Instruction::ADD); // base_ptr + scaled_index

                    // Emit load instruction (result is an rvalue)
                    match element_type {
                        DataType::Char => self.emit(Instruction::LC),
                        DataType::Int | DataType::Ptr(_) => self.emit(Instruction::LI),
                        DataType::Void => return Err(ParserError::TypeError("Cannot index void pointer".to_string())),
                    }
                    left_type = element_type; // Type is the type of the element
                 }
                 Token::Inc | Token::Dec => { // Postfix ++ / --
                     // Requires lvalue
                     let last_op = self.code.last().map(|&op| Instruction::from(op));
                      match last_op {
                          Some(Instruction::LI) | Some(Instruction::LC) => {
                             // 1. Save original address (it's below LI/LC)
                             // Code: ..., LEA/IMM offset, LI/LC
                             // We need to push the address again for the store.
                             self.code.pop(); // Remove LI/LC
                             // Stack has original value loaded by LI/LC
                             // ax has original value
                             // We need: PSH addr, PSH original_val
                             // C4: if (*e == LC) { *e = PSH; *++e = LC; }
                             self.emit(Instruction::PSH); // Push address
                             match last_op {
                                 Some(Instruction::LC) => self.emit(Instruction::LC),
                                 _ => self.emit(Instruction::LI),
                             }
                             // Now stack: [..., address, original_value]
                          }
                          _ => return Err(ParserError::TypeError("Lvalue required for postfix ++/--".to_string())),
                      }

                      self.emit(Instruction::PSH); // Push address again (for store)
                      self.emit_operand(Instruction::IMM, left_type.deref().unwrap_or(DataType::Char).size_of()); // Sizeof element
                      if token == Token::Inc { self.emit(Instruction::ADD); } else { self.emit(Instruction::SUB); }

                      // Store incremented/decremented value
                      match left_type {
                           DataType::Char => self.emit(Instruction::SC),
                           DataType::Int | DataType::Ptr(_) => self.emit(Instruction::SI),
                           _ => panic!("Should be lvalue"),
                       }
                       // Result of postfix is the *original* value. We need to subtract the increment back.
                       self.emit(Instruction::PSH); // Push address (still on stack?) No, need to get it again? C4 pushes value.
                       // C4 logic: Push value, Push IMM size, ADD/SUB, SC/SI, Push IMM size, SUB/ADD
                       // Let's rethink. Stack after load: [..., address]. ax = value.
                       // We need ax to hold original value at the end.
                       // 1. PSH ax (original value)
                       self.emit(Instruction::PSH);
                       // 2. Calculate new value: ax = ax + size
                       self.emit_operand(Instruction::IMM, left_type.deref().unwrap_or(DataType::Char).size_of());
                       if token == Token::Inc { self.emit(Instruction::ADD); } else { self.emit(Instruction::SUB); }
                       // 3. Store new value: PSH addr, ax=new_val, SI/SC
                       // We need the address again. Assume it's below the original value pushed in step 1.
                       // Need to swap stack[top] (original val) and stack[top-1] (address)? Complex.

                       // Let's try C4's emit sequence more directly:
                       // Assume code ends with LEA/IMM addr, LI/LC. ax=value.
                       // C4: if (*e == LC) { *e = PSH; *++e = LC; } else { *e=PSH; *++e=LI; }
                       // This pushes the address, then re-loads the value into ax.
                       let last_op_val = self.code.pop().unwrap(); // LI or LC
                       self.emit(Instruction::PSH); // Push address
                       self.code.push(last_op_val); // Put LI/LC back
                       // Stack: [..., address], ax = value

                       // C4: *++e = PSH;
                       self.emit(Instruction::PSH); // Push value ax
                       // Stack: [..., address, value]

                       // C4: *++e = IMM; *++e = size;
                       self.emit_operand(Instruction::IMM, left_type.deref().unwrap_or(DataType::Char).size_of());
                       // Stack: [..., address, value, size]

                       // C4: *++e = (tk == Inc) ? ADD : SUB;
                       let op = if token == Token::Inc { Instruction::ADD } else { Instruction::SUB };
                       self.emit(op); // ax = value + size
                       // Stack: [..., address]

                       // C4: *++e = (ty == CHAR) ? SC : SI;
                       let store_op = match left_type {
                            DataType::Char => Instruction::SC,
                            _ => Instruction::SI,
                       };
                       self.emit(store_op); // stack[address] = ax (new value). Pops address.
                       // Stack: [...]

                       // C4: *++e = PSH; *++e = IMM; *++e = size;
                       self.emit(Instruction::PSH); // Push new value (still in ax) - NO, C4 pushes size!
                       self.emit_operand(Instruction::IMM, left_type.deref().unwrap_or(DataType::Char).size_of());
                       // Stack: [..., size]

                       // C4: *++e = (tk == Inc) ? SUB : ADD;
                       let op = if token == Token::Inc { Instruction::SUB } else { Instruction::ADD };
                       self.emit(op); // ax = ax - size (restore original value)
                       // Stack: [...]

                       self.consume()?; // Consume ++ / --
                       // Type doesn't change, value in ax is original value
                 }

                // --- Function Call ---
                 Token::LParen => { // Function call
                     let sym_entry = self.find_symbol_for_expression()?; // Get symbol for the function name expression
                     if sym_entry.class != SymbolClass::Fun && sym_entry.class != SymbolClass::Sys {
                        return Err(ParserError::TypeError(format!("'{}' is not a function", sym_entry.name)));
                     }
                     let func_type = sym_entry.data_type.clone(); // Should be function type if we had them
                     let func_class = sym_entry.class;
                     let func_val = sym_entry.value; // Address or syscall number

                     self.consume()?; // Consume '('
                     let mut arg_count = 0;
                     while !self.check(&Token::RParen) {
                         self.parse_expression(Precedence::Assign)?; // Parse argument expression
                         self.emit(Instruction::PSH); // Push argument onto stack
                         arg_count += 1;
                         if !self.check(&Token::RParen) {
                             self.expect(Token::Comma)?;
                         }
                     }
                     self.expect(Token::RParen)?; // Consume ')'

                     // Emit call instruction
                     match func_class {
                        SymbolClass::Sys => {
                            // C4 emits the specific syscall opcode. func_val holds this opcode value.
                            // Instruction::from converts the *integer value* back to the *enum variant*.
                            self.emit(Instruction::from(func_val)); // Convert opcode value to enum
                            ;
                        }
                        SymbolClass::Fun => {
                            // func_val holds the entry address for JSR
                            self.emit_operand(Instruction::JSR, func_val);
                        }
                        _ => unreachable!(),
                    }

                     // Adjust stack pointer to remove arguments
                     if arg_count > 0 {
                         self.emit_operand(Instruction::ADJ, arg_count);
                     }

                     // Result type is function's return type
                     left_type = func_type;
                 }

                 _ => return Err(ParserError::InternalError(format!("Unhandled binary/postfix token: {:?}", token))),
             } // end match token

        } // end loop

        Ok(left_type)
    }



}