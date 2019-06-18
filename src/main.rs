extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use std::fs;
use std::process::exit;

mod parser;

use parser::lex;
use crate::yggl::protocol::Protocol;

#[allow(dead_code)]
mod yggl;

#[allow(unused_imports)]
//mod tests;
#[allow(dead_code)]
fn main() {
    let unparsed_file = fs::read_to_string("discovery.yggl").expect("cannot read file");
    let lex_result = lex(&unparsed_file);
    if let Ok(source) = lex_result {
        match Protocol::from(source) {
            Ok(mut program) => {
                println!(".h:\n{}", program.transpile_header());
                println!(".c:\n{}", program.transpile())
            }
            Err(error) => println!("{}", error)
        }
    } else {
        exit(-1);
    }
}