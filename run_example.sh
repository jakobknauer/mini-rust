#!/bin/bash

# Compile .rs to .ll
cargo run -- "examples/example.rs"

# Create alternative optimized version of .ll (allows easier debugging of control flow etc.)
opt -passes=mem2reg,simplifycfg -S "examples/example.ll" -o "examples/example.ll.opt"

# Compile unoptimized .ll to executable
clang examples/*.ll -o examples/example.out

# Run executable
./examples/example.out

# Print result of executable
echo "$?"
