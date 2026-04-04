# Compile the compiler
cargo build

# Compile Hello World example to LLVM IR
./target/debug/mini-rust --build-dir "examples/build" --crate "hello_world" -- "examples/hello_world.mrs" "stdlib/"*".mrs"

# Compile LLVM IR to executable
clang "examples/build/hello_world.ll" -o "examples/build/hello_world"

# Run Hello World
./examples/build/hello_world
