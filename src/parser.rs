use pest::Parser;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::error::Error;
use pest::error::LineColLocation::*;
use crate::yggl::data::Constant;
use crate::yggl::expression::Expression;
use crate::yggl::statement::Statement;
use crate::yggl::language::Program;

#[derive(Parser)]
#[grammar = "yggl/grammar.pest"]
pub struct YGGLParser;

pub fn lex(code: &str) -> Result<Pairs<Rule>, &str> {
    match YGGLParser::parse(Rule::program, code) {
        Ok(program) => return Ok(program),
        Err(error) => {
            let Error {
                line_col: location,
                ..
            } = error;
            match location {
                Pos((line, col)) => {
                    print!("Unable to parse.\nLine {}, Col{}:", line, col);
                    let problematic_line = code.lines().nth(line - 1).unwrap();
                    println!("{:?}", problematic_line);
                    return Err("TODO");
                }
                _ => {}
            }
        }
    }
    unreachable!();
}

impl<'a> Program<'a> {
    pub fn from(pairs: Pairs<Rule>) -> Result<Program, ()> {
        let mut program = Program::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    if let Ok(statement) = Statement::from(pair) {
                        println!("{}", statement);
                        program.add_statement(statement)
                    } else {
                        panic!("Unable to create statement");
                    }
                }
                Rule::EOI => {}
                _ => unimplemented!()
            }
        }
        return Ok(program);
    }
}

impl<'a> Statement<'a> {
    pub fn from(pair: Pair<Rule>) -> Result<Statement<'a>, ()> {
        println!("Statement: {:?}", pair);
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::atribution => {
                    let mut inner_rules = pair.into_inner();
                    let mut pair = inner_rules.next().unwrap();
                    let identifier = pair.as_str().to_string();
                    pair = inner_rules.next().unwrap();
                    let _ = pair;
                    let expression = Expression::Constant(Constant::Int(1));
                    return Ok(Statement::Assignment(identifier, expression));
                }
                _ => { print!("?: {}", pair) }
            }
        }
        return Err(());
    }
}