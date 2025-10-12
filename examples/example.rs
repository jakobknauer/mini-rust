fn main() -> i32 {
    g(5)
}

fn g(x: i32) -> i32 {
    (x + 7) * 2
}

fn h(b: bool) -> i32 {
    if b { 3 } else { 5 }
}
