#![feature(box_syntax, box_patterns)]
extern crate tyarith;

use tyarith::type_of;
use tyarith::Term::*;

fn main() {
    let program = If(box IsZero(box Pred(box Succ(box Zero))),
                     box Succ(box Succ(box Pred(box Zero))),
                     box Zero);

    println!("Source program:    {}", program);
    println!("Type of program:   {}", type_of(program).expect("Does not type-check!"));
}
