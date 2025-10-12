use std::path::PathBuf;

use colored::Colorize;

fn main() {
    let input_path: PathBuf = std::env::args().nth(1).expect("Please provide a file path").into();

    print_pretty(&format!("Loading source from {}", input_path.to_str().unwrap()));
    let source = std::fs::read_to_string(&input_path).expect("Could not read file");

    let ir = mini_rust::driver::compile(&source, print_pretty).expect("Compilation failed");

    let ir_path = input_path.with_extension("ll");
    print_pretty(&format!("Saving IR to {}", ir_path.display()));
    std::fs::write(&ir_path, ir).expect("Could not write IR file");

    print_pretty("Success");
}

fn print_pretty(msg: &str) {
    println!("{} {}", "::".blue().bold(), msg.bold());
}
