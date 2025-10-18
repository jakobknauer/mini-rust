fn main() -> i32 {
    fac(5)
}

fn g(x: i32) -> i32 {
    x * h(true, true) + h(false, true)
}

fn h(b1: bool, b2: bool) -> i32 {
    if (b1 == b2 & true) | false { 3 } else { 5 }
}

fn fac(n: i32) -> i32 {
    let result = 1;
    loop {
        if n == 0 {
            break;
        } else {};
        result = result * n;
        n = n - 1;
    };
    result
}
