fn main() -> i32 {
    g(2)
}

fn g(x: i32) -> i32 {
    x * h(true) + h(false)
}

fn h(b: bool) -> i32 {
    if b { 3 } else { 5 }
}
