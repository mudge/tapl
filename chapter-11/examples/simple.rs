#![feature(box_syntax)]
extern crate fullsimple;

use fullsimple::{type_of, Term, Context, Type};

fn main() {
    let ctx = Context::new();
    let term = Term::Abs("y".into(),
        Type::Bool,
        box Term::App(
            box Term::Abs(
                "x".into(),
                Type::Product(box Type::Bool, box Type::Unit),
                box Term::Project(box Term::Var(0), 1)
            ),
            box Term::Sequence(box Term::Unit, box Term::Pair(box Term::True, box Term::Unit))
        )
    );

    println!("Source term:    {}", term);
    match type_of(&ctx, &term) {
        Ok(ty) => println!("Resulting type: {}", ty),
        Err(err) => println!("Type error:     {}", err),
    }
}
