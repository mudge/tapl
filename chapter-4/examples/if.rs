#![feature(box_syntax, box_patterns)]
extern crate arith;

use arith::eval;
use arith::Term::*;

fn main() {
    let program = If(box IsZero(box Pred(box Succ(box Zero))),
                     box Succ(box Succ(box Pred(box Zero))),
                     box False);

    println!("Source program:    {}", program);
    println!("Evaluated program: {}", eval(program));
}
