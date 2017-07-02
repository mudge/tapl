extern crate simplebool;

use simplebool::{Context, parse, type_of};

fn main() {
    let ctx = Context::new();
    let program = parse("if ((λy:Bool. 0) true) then ((λz:Bool . 0) if false then ((λz:Bool . 0) true) else false) else ((λz:Bool . 0) if true then false else ((λz:Bool . 0) true))");

    match program {
        Ok(term) => {
            println!("Source term:    {}", term);
            match type_of(&ctx, &term) {
                Ok(ty) => println!("Resulting type: {}", ty),
                Err(err) => println!("Type error:     {}", err),
            }
        }
        Err(e) => println!("Parse error:       {:?}", e),
    }
}
