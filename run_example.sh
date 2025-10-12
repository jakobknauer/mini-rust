#!/bin/bash

cargo run -- "examples/example.rs"
clang examples/*.ll -o examples/example.out
./examples/example.out
echo "$?"
