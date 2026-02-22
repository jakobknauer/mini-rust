#!/bin/bash

set -e

# Compile .rs to .ll
cargo run -- \
    --build-dir "examples/build" \
    --crate "hello_world" \
    -- "examples/hello_world.mrs"

# Create alternative optimized version of .ll (allows easier debugging of control flow etc.)
opt-21 -passes=mem2reg,simplifycfg -S "examples/build/hello_world.ll" -o "examples/build/hello_world.ll.opt"

# Compile unoptimized .ll to executable
clang-21 "examples/build/hello_world.ll" "examples/stdlib.ll" -o "examples/build/hello_world.out"

set +e
# Run executable
./examples/build/hello_world.out

# Print result of executable
echo "$?"
