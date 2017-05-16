extern crate arith;

use std::io::{self, Read};
use arith::{parse, eval};

fn main() {
    let mut buffer = String::new();
    match io::stdin().read_to_string(&mut buffer) {
        Ok(_) => {
            let program = parse(&buffer);

            match program {
                Ok(term) => {
                    println!("Source program:    {}", term);
                    println!("Evaluated program: {}", eval(&term));
                }
                Err(e) => println!("Parse error:       {}", e),
            }
        },
        _ => println!("Could not read from STDIN!"),
    }
}
