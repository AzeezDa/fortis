mod machine;
pub use machine::*;
fn main() {
    let mut m = machine::Machine::new("../out".to_string()).unwrap();
    m.run();
}
