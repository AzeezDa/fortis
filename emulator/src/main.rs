mod machine;
pub use machine::*;
use std::env::args;
fn main() {
    // Must have at least 1 one argument, the file path to a Fortis machine code file
    let mut args = args();
    if let Some(fp) = args.nth(1) {
        match machine::Machine::new(fp) {
            Ok(mut m) => m.run(),
            Err(s) => println!("{}", s.to_string()),
        }
    } else {
        println!("Not file path was provided");
    }
}
