# Roadmap

## Version 1
- ~~Functions, structs, enums, builtin-types (bool and i32 will do for now)~~
- ~~Basic arithmetic and flow control (if, loop)~~
- ~~Parse into AST (no type annotations in functions necessary)~~
- ~~Type checking, transforming to typed IR~~
    - ~~Resolve variables and functions~~
    - ~~Replace operators by functions~~
    - ~~Make temporary values explicit (and define their lifetime using blocks; this makes dropping easier later on)~~
    - ~~Determine all types (bidirectional; no HM-style, as we don't have generics yet)~~
- ~~Generate LLVM IR from typed IR~~
- ~~Missing:~~
    - ~~breaking out of loops~~
    - ~~creating struct and enum values~~
    - ~~accessing struct fields~~
    - ~~equality operators~~
    - ~~logical operators~~
    - ~~more arithmetic operators~~
    - ~~Refine mlr struct handling~~
        - ~~Use field indices instead of field names in field access expressions~~
        - ~~Replace struct expression by multiple assignments~~
    - ~~Decomposition of enum values - `match` or `if let`~~

## Version 2
- ~~References~~
- ~~Higher-order functions / functions as first-class citizens~~
- Generic (without trait bounds)
    - ~~functions~~
    - ~~structs~~
    - ~~enums~~

## Version 3
- Traits and trait bounds
- Closures
- Trait functions call using explicit syntax only (e.g. Add::\<i32\>::add(a, b))
