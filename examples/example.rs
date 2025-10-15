fn main() -> i32 {
    g(2)
}

fn g(x: i32) -> i32 {
    x * h(true, true) + h(false, true)
}

fn h(b1: bool, b2: bool) -> i32 {
    if b1 == b2 { 3 } else { 5 }
}
