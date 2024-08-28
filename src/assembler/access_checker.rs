use super::{compilation::Compilation, syntax::Syntax};

pub fn check_access(compilation: &mut Compilation, syntax: &Vec<Syntax>) {

}

pub fn step_until(compilation: &mut Compilation, syntax: &Vec<Syntax>) {

}


enum RegisterStatus {
    Uncertain, //Cannot be read, only written, to change its state to known
    Known, //Can be read and written to
    Readonly, //Can be read
}