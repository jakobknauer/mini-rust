#!/bin/bash

set -e

# Compile .rs to .ll
cargo run -- \
    --build-dir "examples/build" \
    --crate "example" \
    -- "examples/example.mrs" "examples/option.mrs" "examples/vec.mrs" "examples/iter.mrs"
# Create alternative optimized version of .ll (allows easier debugging of control flow etc.)
opt-21 -passes=mem2reg,simplifycfg -S "examples/build/example.ll" -o "examples/build/example.ll.opt"

# Compile unoptimized .ll to executable
clang-21 "examples/build/"*.ll "examples/stdlib.ll" -o "examples/build/example.out"

set +e
# Run executable
./examples/build/example.out

# Print result of executable
echo "$?"
