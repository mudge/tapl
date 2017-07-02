extern crate fullsimple;

use std::io::{self, Read};

use fullsimple::{parse, type_of, Context};

fn main() {
    let mut buffer = String::new();
    match io::stdin().read_to_string(&mut buffer) {
        Ok(_) => {
            let program = parse(&buffer);

            match program {
                Ok(term) => {
                    let ctx = Context::new();

                    println!("Source program:    {}", term);

                    match type_of(&ctx, &term) {
                        Ok(t) => println!("Type of program:   {}", t),
                        Err(error) => println!("Type error:        {}", error),
                    }
                }
                Err(e) => println!("Parse error:       {:?}", e),
            }
        }
        _ => println!("Could not read from STDIN!"),
    }
}
