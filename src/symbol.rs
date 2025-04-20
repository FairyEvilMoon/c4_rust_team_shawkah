//! Symbol Table structures for the C4 compiler.

use crate::lexer::Token;

// Type representation
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
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
    pub shadowed_value: Option<i64>,
    pub shadowed_scope_level: Option<usize>,
    pub shadowed_type: Option<DataType>,
}