//! Symbol Table structures for the C4 compiler.

// Use crate::lexer::Token instead of the specific team name path
use crate::lexer::Token;
use std::mem; // Needed for size_of

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
    // Matches C4's sizeof logic: char=1, int/ptr=sizeof(i64) assuming 64-bit target
    pub fn size_of(&self) -> i64 {
        match self {
            DataType::Char => mem::size_of::<i8>() as i64, // Typically 1
            DataType::Void => 0, // Sizeof void is often 0 or 1, C4 might treat as 0
            DataType::Int | DataType::Ptr(_) => mem::size_of::<i64>() as i64, // Assume pointers/ints are word-sized (e.g., 8 bytes on 64-bit)
        }
    }
}

// Symbol Class (Variable, Function, etc.)
// Added Key class based on parser usage
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum SymbolClass {
    Key, // Keyword
    Num, // Enum member (numeric constant)
    Fun, // Function
    Sys, // System function/call (builtin)
    Glo, // Global variable
    Loc, // Local variable/parameter
    Enum, // Enum type name (tag) - C4 doesn't use tags heavily
}

// Represents an entry in the symbol table
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: String,
    pub token: Token, // Original token type (Keyword or Id)
    pub class: SymbolClass, // Class of the symbol (Num, Fun, etc.)
    pub data_type: DataType, // Type of the symbol (Int, Char, etc.)
    pub value: i64, // Depends on class: address (Glo/Fun), offset (Loc), enum val (Num), syscall code (Sys)
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
    symbols: Vec<SymbolEntry>,
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
            let scope_to_remove = self.current_scope;
            self.symbols.retain_mut(|entry| {
                if entry.scope_level == scope_to_remove {
                    if let Some(s_class) = entry.shadowed_class {
                        entry.class = s_class;
                        entry.data_type = entry.shadowed_type.clone().unwrap();
                        entry.value = entry.shadowed_value.unwrap();
                        entry.scope_level = entry.shadowed_scope_level.unwrap();
                        entry.shadowed_class = None;
                        entry.shadowed_type = None;
                        entry.shadowed_value = None;
                        entry.shadowed_scope_level = None;
                        true // Keep the restored entry
                    } else {
                        false // Remove the entry entirely
                    }
                } else {
                    true // Keep symbols from outer scopes
                }
            });
            self.current_scope -= 1;
        }
    }

    // Add a new symbol or shadow an existing one from an outer scope.
    pub fn add(
        &mut self,
        name: String,
        token: Token,
        class: SymbolClass,
        data_type: DataType,
        value: i64,
    ) -> Result<&mut SymbolEntry, String> { // Return String error for simplicity

        // Check for redeclaration in the *current* scope first.
        // This avoids ambiguity if a symbol exists in both current and outer scopes.
        if let Some(_) = self.find_in_scope(&name, self.current_scope) {
            return Err(format!(
                "Redeclaration of symbol '{}' in the same scope (level {})",
                name, self.current_scope
            ));
        }

        // Check if a symbol with this name exists (potentially in an outer scope).
        // `rposition` finds the index of the last match (innermost scope where it exists).
        if let Some(existing_index) = self.symbols.iter().rposition(|e| e.name == name) {
            // Now, check if the found symbol is actually in an *outer* scope.
            // If `scope_level == self.current_scope`, it's a redeclaration (handled above).
            if self.symbols[existing_index].scope_level < self.current_scope {
                // --- Shadowing Case ---
                // We are shadowing an outer scope symbol. Modify the existing entry IN PLACE.

                // Borrow temporarily just for modification:
                {
                    let entry_to_modify = &mut self.symbols[existing_index];

                    // Save the outer symbol's details before overwriting.
                    entry_to_modify.shadowed_class = Some(entry_to_modify.class);
                    entry_to_modify.shadowed_type = Some(entry_to_modify.data_type.clone());
                    entry_to_modify.shadowed_value = Some(entry_to_modify.value);
                    entry_to_modify.shadowed_scope_level = Some(entry_to_modify.scope_level);

                    // Update the entry with the new inner scope symbol's details.
                    entry_to_modify.token = token;
                    entry_to_modify.class = class;
                    entry_to_modify.data_type = data_type;
                    entry_to_modify.value = value;
                    entry_to_modify.scope_level = self.current_scope; // Now belongs to the inner scope
                } // `entry_to_modify` borrow ends here.

                // Return a *fresh* mutable borrow using the index.
                // This borrow is valid because it reflects the state *after* modification
                // and is separate from the potential `push` path.
                return Ok(&mut self.symbols[existing_index]);
            }
            // If symbol exists but not in an outer scope, it must be in the current scope,
            // which was already checked and resulted in an error. This path shouldn't
            // logically be hit if the initial check works.
        }

        // --- New Symbol Case ---
        // No symbol found to shadow (or found only in current scope, which is an error handled above).
        // Add a completely new entry to the vector.
        let new_entry = SymbolEntry {
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
        self.symbols.push(new_entry); // This mutable borrow ends immediately after the push.

        // Return a mutable borrow to the *newly added* element. This is safe
        // because the borrow is created *after* the push.
        Ok(self.symbols.last_mut().unwrap())
    }

    // Find a symbol by name, searching from the current scope outwards to global.
    pub fn find(&self, name: &str) -> Option<&SymbolEntry> {
        self.symbols.iter().rfind(|entry| entry.name == name)
    }

    // Find a mutable symbol by name, searching from the current scope outwards.
    pub fn find_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        self.symbols.iter_mut().rfind(|entry| entry.name == name)
    }

    // Find a symbol defined *exactly* in the specified scope.
    pub fn find_in_scope(&self, name: &str, scope: usize) -> Option<&SymbolEntry> {
        self.symbols.iter().find(|entry| entry.name == name && entry.scope_level == scope)
    }

    // Get the current scope level
    pub fn get_current_scope(&self) -> usize {
        self.current_scope
    }
}