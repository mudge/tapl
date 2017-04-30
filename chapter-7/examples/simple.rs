#![feature(box_syntax, box_patterns)]
extern crate untyped;

use untyped::{eval, print_term};
use untyped::Term::*;

fn main() {
    let ctx = Vec::new();
    let term = App(
        box Abs("x".into(), box Var(0)),
        box App(
            box Abs("x".into(), box Var(0)),
            box Abs(
                "z".into(),
                box App(
                    box Abs("x".into(), box Var(1)),
                    box Var(0)
                )
            )
        )
    );
    let evaluated_term = eval(&term);

    println!("Source term:           {}", term);
    println!("Pretty term:           {}", print_term(&ctx, &term));
    println!("Evaluated term:        {}", evaluated_term);
    println!("Pretty evaluated term: {}", print_term(&ctx, &evaluated_term));
}
