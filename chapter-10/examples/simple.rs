#![feature(box_syntax, box_patterns)]
extern crate simplebool;

use simplebool::{type_of, Term, Context, Type};

fn main() {
    let ctx = Context::new();
    let term = Term::App(box Term::Abs("x".into(), Type::Bool, box Term::Var(0)),
                         box Term::If(box Term::True, box Term::True, box Term::False));

    println!("Source term:    {}", term);
    match type_of(&ctx, &term) {
        Ok(ty) => println!("Resulting type: {}", ty),
        Err(err) => println!("Type error:     {}", err),
    }
}
