use pest::Parser;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::error::Error;
use pest::error::LineColLocation::*;
use pest::prec_climber::*;
use crate::yggl::data::Constant;
use crate::yggl::expression::{Expression, BinaryOperation};
use crate::yggl::statement::Statement;
use crate::yggl::language::Program;
use crate::yggl::environment::Variable;
use crate::yggl::function::*;
use std::collections::linked_list::LinkedList;
use std::fmt;

#[derive(Parser)]
#[grammar = "yggl/grammar.pest"]
pub struct YGGLParser;

#[allow(dead_code)]
pub struct CompilationError {
    line: u32,
    col: u32,
    code: String,
    cause: String,
}

impl CompilationError {
    fn new(line: u32, col: u32, code: String, cause: String) -> CompilationError {
        CompilationError {
            line,
            col,
            code,
            cause,
        }
    }
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.col, self.cause)
    }
}

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
    pub fn from(pairs: Pairs<Rule>) -> Result<Program, CompilationError> {
        let mut program = Program::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    let statement = Statement::from(pair)?;
                    program.add_statement(statement);
                }
                Rule::EOI => {}
                _ => unimplemented!()
            }
        }
        Ok(program)
    }
}

impl<'a> Statement<'a> {
    pub fn from(pair: Pair<'a, Rule>) -> Result<Statement<'a>, CompilationError> {
        println!("Statement: {}", pair);
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::atribution => {
                let mut inner_rules = pair.into_inner();
                let mut pair = inner_rules.next().unwrap();
                let identifier = pair.as_str().to_string();
                pair = inner_rules.next().unwrap();
                match pair.as_rule() {
                    Rule::expression => {
                        println!("Expression: {}", pair);
                        let expression = Expression::from(pair).unwrap();
                        return Ok(Statement::Assignment(identifier, expression));
                    }
                    Rule::function => {
                        println!("Function: {}", pair);
                        let function = Function::from(pair)?;
                        return Ok(Statement::FunctionDef(identifier, function));
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
}

lazy_static! {
    static ref BINOP_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left),
            Operator::new(pow, Right)
        ])
    };
}

impl Expression {
    pub fn from(pair: Pair<Rule>) -> Result<Expression, ()> {
        let pairs = pair.into_inner();
        BINOP_CLIMBER.climb(
            pairs,
            |pair: Pair<Rule>|
                match pair.as_rule() {
                    Rule::expression => Expression::from(pair),
                    Rule::number => //TODO Trait std::str::FromStr
                        Ok(Expression::Constant(Constant::parse_number(pair.as_str()))),
                    Rule::identifier => {
                        let var = Variable::new(pair.as_str());
                        Ok(Expression::Variable(var))
                    }
                    _ => unreachable!("\n{:?}\n", pair),
                },
            |lhs: Result<Expression, ()>, op: Pair<Rule>, rhs: Result<Expression, ()>|
                {
                    let l = lhs.unwrap();
                    let r = rhs.unwrap();
                    match op.as_rule() {
                        Rule::add => Ok(l + r),
                        Rule::sub => Ok(l - r),
                        Rule::mul => Ok(l * r),
                        Rule::div => Ok(l / r),
                        Rule::pow => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Pow,
                            Box::new(r))),
                        _ => unreachable!(),
                    }
                },
        )
    }
}