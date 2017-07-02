extern crate arith;

use arith::{parse, eval};

fn main() {
    let program = parse("if iszero pred succ 0 then succ succ pred 0 else false");

    match program {
        Ok(term) => {
            println!("Source program:    {}", term);
            println!("Evaluated program: {}", eval(&term));
        }
        Err(e) => println!("Parse error:       {:?}", e),
    }
}
