#![feature(box_syntax)]
extern crate fullsimple;

use fullsimple::{type_of, Term, Context, Type};

fn main() {
    let ctx = Context::new();
    let term = Term::Case(
        box Term::Inl(
            box Term::Pair(box Term::True, box Term::True),
            Type::Sum(box Type::Product(box Type::Bool, box Type::Bool), box Type::Tuple(vec![Type::Unit, Type::Unit, Type::Unit]))
        ),
        "p".into(),
        box Term::Project(box Term::Var(0), 1),
        "u".into(),
        box Term::False
    );

    println!("Source term:    {}", term);
    match type_of(&ctx, &term) {
        Ok(ty) => println!("Resulting type: {}", ty),
        Err(err) => println!("Type error:     {}", err),
    }
}
