use std::path::PathBuf;

use colored::Colorize;

fn main() {
    let version = env!("CARGO_PKG_VERSION");
    print_pretty(format!("Mini Rust Compiler v{}", version).as_str());
    
    let (major, minor, patch) = inkwell::support::get_llvm_version();
    println!("Inkwell linked to LLVM version {}.{}.{}", major, minor, patch);

    let Some(input_path) = std::env::args().nth(1) else {
        print_error("No input file specified");
        std::process::exit(1);
    };

    print_pretty(&format!("Loading source from {}", input_path));
    let input_path = PathBuf::from(input_path);

    let Ok(source) = std::fs::read_to_string(&input_path) else {
        print_error("Could not read source file");
        std::process::exit(1);
    };

    let ir = match mini_rust::driver::compile(&source, print_pretty) {
        Ok(ir) => ir,
        Err(err) => {
            print_error(&format!("Compilation failed: {}", err));
            std::process::exit(1);
        }
    };

    let ir_path = input_path.with_extension("ll");
    print_pretty(&format!("Saving IR to {}", ir_path.display()));

    let Ok(_) = std::fs::write(&ir_path, ir) else {
        print_error("Could not write IR file");
        std::process::exit(1);
    };

    print_pretty("Success");
}

fn print_pretty(msg: &str) {
    println!("{} {}", "::".blue().bold(), msg.bold());
}

fn print_error(msg: &str) {
    eprintln!("{} {}", "ERROR:".red().bold(), msg.bold());
}
