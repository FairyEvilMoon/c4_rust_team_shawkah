//! Symbol Table structures for the C4 compiler.
use crate::lexer::Token;

// Type representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    Void, // Added for function return types etc.
    Char,
    Int,
    Ptr(Box<DataType>), // Pointer to another type
}

impl DataType {
    // Helper to calculate the "level" of pointer indirection (INT=0, PTR=1, PTR(PTR)=2)
    // Matches C4's ty > PTR logic somewhat.
    pub fn pointer_level(&self) -> usize {
        match self {
            DataType::Void | DataType::Char | DataType::Int => 0,
            DataType::Ptr(inner) => 1 + inner.pointer_level(),
        }
    }

    // Helper to get the base type after dereferencing
    pub fn deref(&self) -> Option<DataType> {
        match self {
            DataType::Ptr(inner) => Some(*inner.clone()),
            _ => None,
        }
    }

     // Helper to create pointer type
     pub fn pointer_to(base: DataType) -> DataType {
         DataType::Ptr(Box::new(base))
     }

     // Size in conceptual memory units (e.g., for pointer arithmetic)
     // Matches C4's sizeof logic: char=1, int/ptr=sizeof(int)
     pub fn size_of(&self) -> i64 {
        match self {
            DataType::Char => std::mem::size_of::<i8>() as i64, // 1
            DataType::Void => 0, // Sizeof void
            DataType::Int | DataType::Ptr(_) => std::mem::size_of::<i64>() as i64, // Assume pointers are word-sized
        }
    }
}

// Symbol Class (Variable, Function, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum SymbolClass {
    Num, // Enum for numeric types
    Fun, // Function
    Sys, // System function/call
    Glo, // Global variable
    Loc, // Local variable
    Enum, // Enum type
}

// Represents an entry in the symbol table
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: String,
    pub token: Token, // Original token type (Keyword or Id)
    pub class: SymbolClass, // Class of the symbol (Num, Fun, etc.)
    pub data_type: DataType, // Type of the symbol (Int, Char, etc.)
    pub value: i64, // Depends on class: address (Glo), offset (Loc), etc.
    pub scope_level: usize, // Scope level (0 for global, >0 for local)

    // Handling shadowing/local scopes
    pub shadowed_class: Option<SymbolClass>,
    pub shadowed_type: Option<DataType>,
    pub shadowed_value: Option<i64>,
    pub shadowed_scope_level: Option<usize>,
}

// Symbol Table Implementation
#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: Vec<SymbolEntry>, // Vector of symbols
    current_scope: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: Vec::new(),
            current_scope: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
    }
    pub fn leave_scope(&mut self) {
        if self.current_scope > 0 {
            // Read current scope level before the mutable borrow starts
            let scope_to_remove = self.current_scope;

            // Restore shadowed symbols
            self.symbols.retain_mut(|entry| {
                if entry.scope_level == scope_to_remove {
                // This symbol is going out of scope. If it shadowed something, restore it.
                if let Some(s_class) = entry.shadowed_class {
                    entry.class = s_class;
                    entry.data_type = entry.shadowed_type.clone().unwrap();
                    entry.value = entry.shadowed_value.unwrap();
                    entry.scope_level = entry.shadowed_scope_level.unwrap();
                    // Clear shadowing info
                    entry.shadowed_class = None;
                    entry.shadowed_type = None;
                    entry.shadowed_value = None;
                    entry.shadowed_scope_level = None;
                    true // Keep the restored entry
                } else {
                    false // Remove the entry entirely if it didn't shadow anything
                }
            } else {
                true // Keep symbols from outer scopes
            }
        });
        self.current_scope -= 1;
        }
    }

    // Add a new symbol to the table or shadow an existing one
    pub fn add(
        &mut self,
        name: String,
        token: Token,
        class: SymbolClass,
        data_type: DataType,
        value: i64,
    ) -> Result<&mut SymbolEntry, String> {
        // Check for existing symbol in the current scope
        if let Some(existing) = self.find_in_scope(&name, self.current_scope) {
            // Allow shadowing globals, but not redeclaring locals/params in the same scope
            if existing.scope_level == self.current_scope {
                return Err(format!("Redeclaration of symbol '{}' in the same scope", name));
            }
            // Shadowing: save the old symbol's class, value, and scope level
            let entry = self.find_mut(&name).unwrap();
            entry.shadowed_class = Some(entry.class);
            entry.shadowed_type = Some(entry.data_type.clone());
            entry.shadowed_value = Some(entry.value);
            entry.shadowed_scope_level = Some(entry.scope_level);
            entry.class = class;
            entry.data_type = data_type;
            entry.value = value;
            entry.scope_level = self.current_scope;
            entry.token = token;
            Ok(entry)
        } else {
            // New symbol: add it to the table
            let entry = SymbolEntry {
                name,
                token,
                class,
                data_type,
                value,
                scope_level: self.current_scope,
                shadowed_class: None,
                shadowed_type: None,
                shadowed_value: None,
                shadowed_scope_level: None,
            };
            self.symbols.push(entry);
            Ok(self.symbols.last_mut().unwrap())
        }
    }
    // Find a symbol searching from current scope outwards
    pub fn find(&self, name: &str) -> Option<&SymbolEntry> {
        self.symbols.iter().rfind(|entry| entry.name == name)
    }

    // Find a symbol mutable, searching from current scope outwards
    pub fn find_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        self.symbols.iter_mut().rfind(|entry| entry.name == name)
    }

    // Find a symbol in a specific scope only
    pub fn find_in_scope(&self, name: &str, scope: usize) -> Option<&SymbolEntry> {
        self.symbols.iter().find(|entry| entry.name == name && entry.scope_level == scope)
    }

    // Get all symbols in the current scope
    pub fn get_current_scope(&self) -> usize {
        self.current_scope
    }
}