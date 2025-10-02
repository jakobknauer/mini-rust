fn main() {
    let path = std::env::args().nth(1).expect("Please provide a file path");
    let source = std::fs::read_to_string(path).expect("Could not read file");
    _ = mini_rust::driver::compile(&source);
}
