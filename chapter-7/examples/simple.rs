#![feature(box_syntax, box_patterns)]
extern crate untyped;

use untyped::{eval, print_term};
use untyped::Term::*;

fn main() {
    let ctx = Vec::new();
    let term = App(
        box Abs("x".to_owned(), box Var(0)),
        box App(
            box Abs("x".to_owned(), box Var(0)),
            box Abs(
                "z".to_owned(),
                box App(
                    box Abs("x".to_owned(), box Var(1)),
                    box Var(0)
                )
            )
        )
    );

    println!("Source term:    {}", term);
    println!("Evaluated term: {}", eval(&term));
    println!("Pretty term:    {}", print_term(&ctx, &term));
}
