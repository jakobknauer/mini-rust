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

    let Some(build_path) = std::env::args().nth(2) else {
        print_error("No build directory specified");
        std::process::exit(1);
    };

    let input_path = PathBuf::from(input_path);
    let build_path = PathBuf::from(build_path);

    println!("Loading source from {}", input_path.as_os_str().to_str().unwrap());

    let Ok(source) = std::fs::read_to_string(&input_path) else {
        print_error("Could not read source file");
        std::process::exit(1);
    };

    let input_stem = input_path.file_stem().unwrap();

    let output_paths = mini_rust::driver::OutputPaths {
        mlr: Some(&build_path.join(input_stem).with_extension("mlr")),
        llvm_ir: Some(&build_path.join(input_stem).with_extension("ll")),
    };

    if let Err(err) = mini_rust::driver::compile(&source, print_pretty, print_detail, &output_paths) {
        print_error(&format!("Compilation failed: {}", err));
        std::process::exit(1);
    };

    print_pretty("Success");
}

fn print_pretty(msg: &str) {
    println!("{} {}", "::".blue().bold(), msg.bold());
}

fn print_detail(msg: &str) {
    println!("{}", msg);
}

fn print_error(msg: &str) {
    eprintln!("{} {}", "ERROR:".red().bold(), msg.bold());
}
