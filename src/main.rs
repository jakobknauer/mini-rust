use std::path::PathBuf;

use colored::Colorize;

use clap::Parser;

#[derive(Parser)]
#[command(version)]
struct Cli {
    input: Vec<PathBuf>,
    #[arg(short, long)]
    build_dir: PathBuf,
    #[arg(short, long)]
    crate_: String,
}

fn main() {
    let cli = Cli::parse();

    print_pretty(&format!("Mini Rust Compiler v{}", env!("CARGO_PKG_VERSION")));

    let (major, minor, patch) = inkwell::support::get_llvm_version();
    println!("Inkwell linked to LLVM version {}.{}.{}", major, minor, patch);

    let sources: Vec<_> = cli
        .input
        .into_iter()
        .inspect(|path| println!("Loading source from {}", path.as_os_str().to_str().unwrap()))
        .map(|path| {
            let Ok(source) = std::fs::read_to_string(path) else {
                print_error("Could not read source file");
                std::process::exit(1);
            };

            source
        })
        .collect();

    let output_paths = mini_rust::driver::OutputPaths {
        mlr: Some(&cli.build_dir.join(&cli.crate_).with_extension("mlr")),
        llvm_ir: Some(&cli.build_dir.join(&cli.crate_).with_extension("ll")),
    };

    if let Err(err) = mini_rust::driver::compile(&sources, print_pretty, print_detail, &output_paths) {
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
    eprintln!("{} {}.", "ERROR:".red().bold(), msg.bold());
}
