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

The main entry point is [main.rs](src/main.rs). The compilation process in orchestrated by the [driver](src/driver.rs) module.
The compiler currently consists of two parallel pipelines, the second of which is incomplete and bound to replace the first.

Common:
- Parse the [Abstract Syntax Tree (AST)](src/ast.rs) from source code in [parser](src/parse.rs).
- Build the `Ctxt` object defined in [ctxt](src/ctxt.rs) from the AST. This serves as a central registry for types, functions, traits etc..

"Old" pipeline:
- Lower the AST to [Mid-Level Representation (MLR)](src/ctxt/mlr.rs) in [ast_lowering](src/ast_lowering.rs). This pass includes type inference and checking, name resolution, desugaring, as well as the actual lowering.
- Monomorphization: Using information recorded during the previous pass, recursively determine all instantiations of functions with respective generic arguments.
- Lower the MLR to LLVM Immediate Representation (IR) in [mlr_lowering](src/mlr_lowering.rs). This is done for all function instantiations, i.e. this is where the actual monomorphization happens.
- Write the LLVM IR in (textual representation) to disk.

"New" pipeline (in progress):
- Lower the AST to [High-Level Representation (HLR)](src/hlr.rs) in [ast_to_hlr](src/ast_to_hlr.rs). This includes resolution of most names, e.g. variables and functions, but not e.g. method calls, as this requires typechecking the receiver. Syntax desugaring (e.g. `while` to `loop`) also takes place here. (complete)
- Typecheck the HLR in [typeck](src/typeck.rs). (mostly complete)
- Lower the HLR to MLR. (to be done)
- Proceed as in the old pipeline

The compiler uses the following representations of the program:
- [AST](src/ast.rs): Mostly verbatim representation of the source code, except for comments, whitespaces and some details. Purely syntactical. The only representation that contains all parts of the program, not only function bodies.
- [HLR](src/hlr.rs): Slightly more abstract representation of the program, where names are resolved and syntax desugaring has been performed. Serves as basis for typechecking in the new pipeline, but type information is stored externally.
- [MLR](src/ctxt/mlr.rs): Intermediate representation of the program, where all types are determined, all temporary values are made explicit, and all expressions are separated into Places, Values, and Operands.
- [LLVM IR](https://llvm.org/docs/LangRef.html)


## Contributing

The repository contains [a custom pre-commit hook](.githooks/pre-commit). To enable, run
```bash
git config --local core.hooksPath .githooks
```
