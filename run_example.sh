#!/bin/bash

set -e

# Compile .rs to .ll
cargo run -- "examples/example.mrs" "examples/build"

# Create alternative optimized version of .ll (allows easier debugging of control flow etc.)
opt-18 -passes=mem2reg,simplifycfg -S "examples/build/example.ll" -o "examples/build/example.ll.opt"

# Compile unoptimized .ll to executable
clang-18 "examples/build/"*.ll "examples/stdlib.ll" -o "examples/build/example.out"

set +e
# Run executable
./examples/build/example.out

# Print result of executable
echo "$?"
