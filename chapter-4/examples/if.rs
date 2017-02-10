extern crate arith;

use arith::eval;
use arith::Term::*;

fn main() {
    let program = If(Box::new(IsZero(Box::new(Pred(Box::new(Succ(Box::new(Zero))))))),
                     Box::new(Succ(Box::new(Succ(Box::new(Zero))))),
                     Box::new(False));

    println!("Source program:    {}", program);
    println!("Evaluated program: {}", eval(program));
}
