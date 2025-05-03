//! Symbol Table structures for the C4 compiler, enhanced for self-hosting.

use crate::lexer::Token;
use std::{collections::HashMap, mem}; // Using HashMap for faster lookups might be better eventually

// --- Type Representation ---
#[derive(Debug, Clone, PartialEq, Eq, Hash)] // Added Hash for potential HashMap use
pub enum DataType {
    Void,
    Char,
    Int,
    Ptr(Box<DataType>), // Pointer to another type
    // Consider adding Func type if needed: Func(Box<DataType>, Vec<DataType>) for return type and args
}

impl DataType {
    pub fn is_pointer(&self) -> bool {
        matches!(self, DataType::Ptr(_))
    }

    /// Gets the type pointed to. Returns None if not a pointer.
    pub fn deref(&self) -> Option<DataType> {
        match self {
            DataType::Ptr(inner) => Some(*inner.clone()),
            _ => None,
        }
    }

     /// Creates a pointer type pointing to the given base type.
    pub fn pointer_to(base: DataType) -> DataType {
        DataType::Ptr(Box::new(base))
    }

    /// Size in memory words/units used by the VM.
    /// Assumes 32-bit VM where int/ptr take 1 word, char ideally takes 1 byte but often stored in a word.
    /// C4 uses sizeof(int) for pointers. Let's assume word size = 1 for simplicity here, like original C4.
    /// A more realistic VM might use byte sizes.
    pub fn size_in_words(&self) -> i64 {
         match self {
            DataType::Void => 0, // Void has no size
            // C4 logic: char is 1 byte, int/ptr is word size (assumed 1 word here)
            DataType::Char => 1, // Takes 1 word for storage even if only byte is used by LC/SC
            DataType::Int | DataType::Ptr(_) => 1, // Ints and Pointers take one VM word (e.g., i32)
         }
    }

    /// Size in bytes, more conventional C sizeof.
    pub fn size_in_bytes(&self) -> i64 {
        match self {
            DataType::Char => mem::size_of::<i8>() as i64, // Usually 1
            DataType::Void => 0, // Often 0 or 1, let's use 0
             // Assume VM uses i32 for Value, so pointers and ints are that size
            DataType::Int | DataType::Ptr(_) => mem::size_of::<crate::vm::Value>() as i64,
         }
    }
}

// --- Symbol Class (Variable, Function, etc.) ---
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)] // Added Hash
pub enum SymbolClass {
    Key,    // Keyword (reserved identifier)
    Num,    // Enum member (numeric constant)
    Fun,    // Function definition
    Sys,    // System function/call (builtin)
    Glo,    // Global variable
    Loc,    // Local variable / function parameter
    Enum,   // Enum type name (tag)
    // Add others if needed: Typ (typedef), Stt (struct tag), Fld (struct field)
}

// --- Symbol Table Entry ---
// Represents an entry in the symbol table
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: String,
    // pub token: Token, // Original token might be useful, but name/class usually suffice
    pub class: SymbolClass,    // Class of the symbol
    pub data_type: DataType, // Type of the symbol
    pub value: i64,          // Depends on class: address (Glo/Fun), offset (Loc), enum val (Num), syscall code (Sys)
    pub scope_level: usize,  // Scope level (0 for global, >0 for local)

    // Information about the symbol this one is shadowing (if any)
    // Used to restore outer symbols when leaving scope.
    pub shadowed_class: Option<SymbolClass>,
    pub shadowed_type: Option<DataType>,
    pub shadowed_value: Option<i64>,
    pub shadowed_scope_level: Option<usize>,
}

// --- Symbol Table Implementation ---
// Uses a Vec for simplicity, mimicking C4's linear scan.
// For larger projects, HashMap<String, Vec<SymbolEntry>> keyed by scope might be better.
#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: Vec<SymbolEntry>, // Simple list, newest entries added at the end
    current_scope: usize,      // 0 = global, 1 = first level function scope, etc.
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            symbols: Vec::new(),
            current_scope: 0, // Start at global scope
        }
    }

    /// Enters a new lexical scope (e.g., function body, block).
    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
        // println!("DEBUG: Entered scope {}", self.current_scope); // Debug print
    }

    /// Leaves the current lexical scope, removing/unshadowing symbols.
    pub fn leave_scope(&mut self) {
        if self.current_scope == 0 {
             // eprintln!("Warning: Attempted to leave global scope.");
            return;
        }
        // println!("DEBUG: Leaving scope {}", self.current_scope); // Debug print

        let scope_to_remove = self.current_scope;
        self.current_scope -= 1; // Decrement scope level first

        // Iterate backwards to efficiently remove or unshadow
        let mut i = self.symbols.len();
        while i > 0 {
            i -= 1;
            let entry = &mut self.symbols[i]; // Need mutable borrow

            if entry.scope_level == scope_to_remove {
                // This symbol was defined in the scope we are leaving
                if entry.shadowed_class.is_some() {
                     // It was shadowing something; restore the shadowed symbol's info
                    // println!("DEBUG: Unshadowing '{}' back to scope {}", entry.name, entry.shadowed_scope_level.unwrap_or(99));
                    entry.class = entry.shadowed_class.take().unwrap();
                    entry.data_type = entry.shadowed_type.take().unwrap();
                    entry.value = entry.shadowed_value.take().unwrap();
                    entry.scope_level = entry.shadowed_scope_level.take().unwrap();
                     // Clear shadow fields again just in case
                    entry.shadowed_class = None;
                    entry.shadowed_type = None;
                    entry.shadowed_value = None;
                    entry.shadowed_scope_level = None;

                } else {
                    // It wasn't shadowing; remove it completely
                    // println!("DEBUG: Removing '{}' from scope {}", entry.name, scope_to_remove);
                    self.symbols.remove(i);
                }
            } else if entry.scope_level > scope_to_remove {
                 // This should not happen if scopes are managed correctly
                 eprintln!("Warning: Found symbol '{}' with scope level {} while leaving scope {}", entry.name, entry.scope_level, scope_to_remove);
            }
            // Symbols from outer scopes (scope_level < scope_to_remove) are kept as is.
        }
         // println!("DEBUG: Now in scope {}", self.current_scope); // Debug print
    }

     /// Adds a new symbol or shadows an existing one from an outer scope.
     /// Returns Ok(()) on success, Err(message) on failure (e.g., redeclaration).
    pub fn add(
        &mut self,
        name: String,
        token: Token, // Keep token for context if needed
        class: SymbolClass,
        data_type: DataType,
        value: i64,
    ) -> Result<&mut SymbolEntry, String> {

        // Search for existing symbol with the same name *starting from the end* (current scope outward)
        if let Some(existing_index) = self.symbols.iter().rposition(|e| e.name == name) {
            // Found an existing symbol with this name. Check its scope.
            let existing_entry = &self.symbols[existing_index];

            if existing_entry.scope_level == self.current_scope {
                // Redeclaration in the *same* scope. This is an error.
                return Err(format!(
                    "Redeclaration of symbol '{}' in the same scope (level {})",
                    name, self.current_scope
                ));
            } else if existing_entry.scope_level < self.current_scope {
                // --- Shadowing Case ---
                // Found in an outer scope. We need to overwrite it *in place* temporarily.
                // Need mutable access here.
                let entry_to_modify = &mut self.symbols[existing_index];

                // Store the details of the symbol being shadowed
                entry_to_modify.shadowed_class = Some(entry_to_modify.class);
                entry_to_modify.shadowed_type = Some(entry_to_modify.data_type.clone());
                entry_to_modify.shadowed_value = Some(entry_to_modify.value);
                entry_to_modify.shadowed_scope_level = Some(entry_to_modify.scope_level);

                // Update the entry with the new inner scope symbol's details
                entry_to_modify.class = class;
                entry_to_modify.data_type = data_type;
                entry_to_modify.value = value;
                entry_to_modify.scope_level = self.current_scope; // Belongs to the current scope now

                // println!("DEBUG: Shadowing '{}' in scope {}", name, self.current_scope); // Debug
                // Return mutable ref to the modified entry
                return Ok(entry_to_modify);

            }
            // else: existing_entry.scope_level > self.current_scope - Should not happen
        }

        // --- New Symbol Case ---
        // No symbol with this name found in current or outer scopes. Add a new entry.
        // println!("DEBUG: Adding new symbol '{}' in scope {}", name, self.current_scope); // Debug
        let new_entry = SymbolEntry {
            name,
            class,
            data_type,
            value,
            scope_level: self.current_scope,
            shadowed_class: None,
            shadowed_type: None,
            shadowed_value: None,
            shadowed_scope_level: None,
        };
        self.symbols.push(new_entry);
        Ok(self.symbols.last_mut().unwrap()) // Return mutable ref to the newly added entry
    }

     /// Adds a reserved keyword to the symbol table.
     /// Keywords don't have type/value/scope in the same way variables do.
    pub fn add_reserved(&mut self, name: &str, token: Token) -> Result<(), String> {
        // Check if already exists (shouldn't happen for keywords)
        if self.find(name).is_some() {
            return Err(format!("Keyword '{}' seems to be already defined.", name));
        }
        let entry = SymbolEntry {
            name: name.to_string(),
            class: SymbolClass::Key,
            data_type: DataType::Void, // Type irrelevant for keywords
            value: 0,                 // Value irrelevant
            scope_level: 0,           // Keywords are global
            shadowed_class: None,
            shadowed_type: None,
            shadowed_value: None,
            shadowed_scope_level: None,
        };
        self.symbols.push(entry);
        Ok(())
    }


    /// Finds a symbol by name, searching from the current scope outwards.
    pub fn find(&self, name: &str) -> Option<&SymbolEntry> {
        // Iterate backwards (from end of vec) to find the innermost scope's definition first
        self.symbols.iter().rfind(|entry| entry.name == name)
    }

    /// Finds a mutable symbol reference by name.
    pub fn find_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
         self.symbols.iter_mut().rfind(|entry| entry.name == name)
    }

    /// Finds a symbol defined *exactly* in the specified scope.
    pub fn find_in_scope(&self, name: &str, scope: usize) -> Option<&SymbolEntry> {
        // Iterate backwards might be slightly faster if symbols are somewhat ordered by scope
        self.symbols.iter().rfind(|entry| entry.name == name && entry.scope_level == scope)
    }

    /// Gets the current scope level.
    pub fn get_current_scope(&self) -> usize {
        self.current_scope
    }
}