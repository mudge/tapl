#[macro_use(arith)]
extern crate arith;

use arith::{eval, Term};

fn main() {
    let program = arith! { if (iszero pred succ 0) then (succ succ pred 0) else (false) };

    println!("Source program:    {}", program);
    println!("Evaluated program: {}", eval(&program));
}
