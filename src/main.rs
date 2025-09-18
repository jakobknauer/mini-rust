fn main() {
    let path = std::env::args().nth(1).unwrap();
    let source = std::fs::read_to_string(path).unwrap();
    mini_rust::driver::compile(&source);
}
