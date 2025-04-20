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