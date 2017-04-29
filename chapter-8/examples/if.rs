#![feature(box_syntax, box_patterns)]
extern crate tyarith;

use tyarith::type_of;
use tyarith::Term::*;

fn main() {
    let program = If(box IsZero(box Pred(box Succ(box Zero))),
                     box Succ(box Succ(box Pred(box True))),
                     box Zero);

    println!("Source program:    {}", program);
    match type_of(&program) {
        Ok(t) => println!("Type of program:   {}", t),
        Err(error) => println!("Type error:        {}", error),
    }
}
