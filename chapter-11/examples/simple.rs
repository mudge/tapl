#![feature(box_syntax)]
extern crate fullsimple;

use fullsimple::{Context, parse, type_of};

fn main() {
    let ctx = Context::new();
    let program = parse("case inl {true, true} as Bool*Bool+{Unit,Unit,Unit} of inl p => 0.1 | \
                         inr u => false");

    match program {
        Ok(term) => {
            println!("Source term:    {}", term);
            match type_of(&ctx, &term) {
                Ok(ty) => println!("Resulting type: {}", ty),
                Err(err) => println!("Type error:     {}", err),
            }
        }
        Err(e) => println!("Parse error: {:?}", e),
    }
}
