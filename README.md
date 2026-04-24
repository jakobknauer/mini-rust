# mini-rust

This is an experimental compiler for (something that resembles) a subset of the Rust programming language,
written in Rust.

Supported features include:
- Basic syntax (functions, variables, control flow with `if/else`, `match`, `loop`, `while`, `for` etc.)
- Type inference within functions
- Primitive types, tuples, structs and enums, references and raw pointers, function pointers, never type
- Traits and impls, including associated types and functions, and default method implementations
- Generics (functions, structs and enums, traits and impls) with constraints (for functions, methods, and impls)
- Opaque return types (`impl Trait`/`impl Fn`)
- Closures capturing local variables
- Pattern matching with recursive patterns: structs, enum variants, tuples, constants, variables (bindings), references, and wilcards
- Operator overloading:
    - Binary arithmetic/logical/bit operations: `+`, `-`, `*`, `/`, `%`, `&`, `|` via `Add`, `Sub` etc. traits
    - `==` and `!=` via the `Eq` trait, providing a default impl for `!=`
    - Unary `*` via the `Deref` trait, also used for deref coercion

As backend LLVM is used via the [inkwell](https://github.com/TheDan64/inkwell) library.

Major missing features include:
- Lifetimes and borrow checking
- Packages and crates
- Metaprogramming (macros)


## Usage

From [run_hello_world.sh](run_hello_world.sh):

```bash
# Compile the compiler
cargo build

# Compile Hello World example to LLVM IR
./target/debug/mini-rust --build-dir "examples/build" --crate "hello_world" -- "examples/hello_world.mrs" "stdlib/"*".mrs"

# Compile LLVM IR to executable
clang "examples/build/hello_world.ll" -o "examples/build/hello_world"

# Run Hello World
./examples/build/hello_world
```

Check or run [run_example.sh](run_example.sh) for another example.
See the [examples](examples) and [stdlib](stdlib) directory for compilable code examples.

## Architecture

The main entry point is [main.rs](src/main.rs). The compilation process in orchestrated by the [driver](src/driver.rs) module.

The compilation process takes the following steps:
- Parse the [Abstract Syntax Tree (AST)](src/ast.rs) from source code in [parser](src/parse.rs).
- Build the `Ctxt` object defined in [ctxt](src/ctxt.rs) from the AST. This serves as a central registry for types, functions, traits etc..
- Lower the AST to [High-Level Representation (HLR)](src/hlr.rs) in [ast_lowering](src/ast_lowering.rs). This includes resolution of most names, e.g. variables and functions, but not e.g. method calls, as this requires typechecking the receiver. Syntax desugaring (e.g. `while` and `for` to `loop`) also takes place here.
- Typecheck the HLR in [typeck](src/typeck.rs).
- Lower the HLR to [Mid-Level Representation (MLR)](src/mlr.rs) in [hlr_lowering](src/hlr_lowering.rs).
- Check mutability in [mutck](src/mutck.rs): verify that only mutable bindings are assigned to, and that `&mut` references are only taken of mutable places.
- Monomorphization: Using information recorded during the previous pass, recursively determine all instantiations of functions with respective generic arguments.
- Lower the MLR to LLVM Immediate Representation (IR) in [mlr_lowering](src/mlr_lowering.rs). This is done for all function instantiations, i.e. this is where the actual monomorphization happens.
- Write the LLVM IR in (textual representation) to disk.

The compiler uses the following representations of the program:
- [AST](src/ast.rs): Mostly verbatim representation of the source code, except for comments, whitespaces and some details. Purely syntactical. The only representation that contains all parts of the program, not only function bodies.
- [HLR](src/hlr.rs): Slightly more abstract representation of the program, where names are resolved and syntax desugaring has been performed. Serves as basis for typechecking in the new pipeline, but type information is stored externally.
- [MLR](src/mlr.rs): Intermediate representation of the program, where all types are determined, all temporary values are made explicit, and all expressions are separated into Places, Values, and Operands.
- [LLVM IR](https://llvm.org/docs/LangRef.html)


## Contributing

The repository contains [a custom pre-commit hook](.githooks/pre-commit). To enable, run
```bash
git config --local core.hooksPath .githooks
```
