# C4 Compiler Rewrite in Rust

This project is a rewrite of the original ["C in 4 Functions" (C4)](https://github.com/rswier/c4) compiler from C to Rust. It aims to maintain functional equivalence with the original, including its self-hosting capability, while leveraging Rust's safety features and modern programming idioms.

## Objective

Rewrite the C4 compiler (lexer, parser, virtual machine) in Rust, ensuring it compiles the same subset of C as the original C4, including the C4 source code itself (`examples/c4.c`). The project emphasizes Rust's safety, idiomatic usage, collaboration via Git/GitHub, testing, and documentation.

## Features

*   Compiles a subset of C language.
*   Targets a simple virtual machine.
*   Self-hosting: The Rust version can compile the original `c4.c` source code.
*   Written in idiomatic Rust with safety guarantees.
*   Unit and integration tests.
*   Documentation generated via `cargo doc`.
*   **(Optional Bonus Feature)** [Describe bonus feature if implemented, e.g., Added support for basic floating-point arithmetic (float type, literals, +,-,*,/ operations)]
