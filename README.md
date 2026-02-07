# mini-rust

This is an experimental compiler for (something that resembles) a subset of the Rust programming language,
written in Rust.

Supported features include:
- Basic syntax (functions, variables, control flow etc.)
- Type inference within functions
- Primitive types, tuples, structs and enums, references and raw pointers, function pointers
- Generics (functions, structs and enums, traits and impls) with constraints (for functions and methods)
- Closures capturing local variables
- Traits and impls, including associated types
- Pattern matching (for enums and references to enums only)

As backend LLVM is used via the [inkwell](https://github.com/TheDan64/inkwell) library.

Major missing features include:
- Mutability vs immutability
- Lifetimes and borrow checking
- Packages and crates
- Metaprogramming (macros)


## Usage

See [run_example.sh](run_example.sh) for an example on how to use mini-rust.
See the [examples](examples) directory for compilable code examples.

The compiler as of now outputs LLVM IR, which can be compiled to machine code using clang.

## Architecture

TODO


## Contributing

The repository contains [a custom pre-commit hook](.githooks/pre-commit). To enable, run
```bash
git config --local core.hooksPath .githooks
```
