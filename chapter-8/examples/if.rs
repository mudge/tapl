#[macro_use(tyarith)]
extern crate tyarith;

use tyarith::{type_of, Term};

fn main() {
    let program = tyarith! { if (iszero pred succ 0) then (succ succ pred true) else (0) };

    println!("Source program:    {}", program);
    match type_of(&program) {
        Ok(t) => println!("Type of program:   {}", t),
        Err(error) => println!("Type error:        {}", error),
    }
}
