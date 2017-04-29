#![feature(box_syntax, box_patterns)]
extern crate untyped;

use untyped::eval;
use untyped::Term::*;

fn main() {
    let term = App(
        box Abs(
            box Abs(
                box App(
                    box Var(1),
                    box Var(0)
                )
            )
        ),
        box Abs(
            box Var(2)
        )
    );

    println!("Source term:    {}", term);
    println!("Evaluated term: {}", eval(&term));
}
