# Roadmap

## Version 1
- Functions, structs, enums, builtin-types (bool and i32 will do for now)
- Basic arithmetic and flow control (if, loop)
- Parse into AST (no type annotations in functions necessary)
- Type checking (Hindley-Milner style), transforming to typed IR
- Generate LLVM IR from typed IR

## Version 2
- Higher-order functions / functions as first-class citizens
- Generic functions and structs (without trait bounds)

## Version 3
- Traits and trait bounds
- Closures
- Trait functions call using explicit syntax only (e.g. Add::<i32>::add(a, b))
