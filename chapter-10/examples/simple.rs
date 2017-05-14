#[macro_use(simplebool)]
extern crate simplebool;

use simplebool::{type_of, Term, Context, Type};

fn main() {
    let ctx = Context::new();
    let term = simplebool! {
        (if ((λ(y: Bool) . 0) true) then
            ((λ(z: Bool) . 0) (if false then ((λ(z: Bool) . 0) true) else false)) else
            ((λ(z: Bool) . 0) (if true then false else ((λ(z: Bool) . 0) true))))
    };

    println!("Source term:    {}", term);
    match type_of(&ctx, &term) {
        Ok(ty) => println!("Resulting type: {}", ty),
        Err(err) => println!("Type error:     {}", err),
    }
}
