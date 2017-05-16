extern crate tyarith;

use tyarith::{parse, type_of};

fn main() {
    let program = parse("if iszero pred succ 0 then succ succ pred true else 0");

    match program {
        Ok(term) => {
            println!("Source program:    {}", term);

            match type_of(&term) {
                Ok(t) => println!("Type of program:   {}", t),
                Err(error) => println!("Type error:        {}", error),
            }
        }
        Err(e) => println!("Parse error:        {}", e),
    }
}
