use std::path::PathBuf;

fn main() {
    let input_path: PathBuf = std::env::args().nth(1).expect("Please provide a file path").into();
    let source = std::fs::read_to_string(&input_path).expect("Could not read file");

    let ir = mini_rust::driver::compile(&source).expect("Compilation failed");

    let ir_path = input_path.with_extension("ll");
    std::fs::write(&ir_path, ir).expect("Could not write IR file");

    println!("Wrote IR to {}", ir_path.display());
}
