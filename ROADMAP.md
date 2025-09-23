# Roadmap

## Version 1
- ~~Functions, structs, enums, builtin-types (bool and i32 will do for now)~~
- ~~Basic arithmetic and flow control (if, loop)~~
- ~~Parse into AST (no type annotations in functions necessary)~~
- Type checking, ~~transforming to typed IR~~
    - ~~Resolve variables and functions~~
    - ~~Replace operators by functions~~
    - ~~Make temporary values explicit (and define their lifetime using blocks; this makes dropping easier later on)~~
    - Determine all types (bidirectional; no HM-style, as we don't have generics yet)
- Generate LLVM IR from typed IR

## Version 2
- Higher-order functions / functions as first-class citizens
- Generic functions and structs (without trait bounds)

## Version 3
- Traits and trait bounds
- Closures
- Trait functions call using explicit syntax only (e.g. Add::\<i32\>::add(a, b))
