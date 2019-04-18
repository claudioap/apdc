extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;
use std::process::exit;

mod parser;
use parser::lex;

mod yggl;
use yggl::language::*;

#[allow(dead_code)]
#[allow(unused_imports)]
fn main() {

    let unparsed_file = fs::read_to_string("test.yggl").expect("cannot read file");
    let lex_result = lex(&unparsed_file);

    if let Ok(program) = lex_result{
        if let Ok(mut program) = Program::from(program){
            println!("{}", program.transpile());
        }
    }else {
        exit(-1);
    }
}