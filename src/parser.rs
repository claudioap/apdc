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
                    println!("Pair: {:?}", pair);
                    let expression = Expression::from(pair).unwrap();//Expression::Constant(Constant::Int(1));
                    return Ok(Statement::Assignment(identifier, expression));
                }
                _ => { print!("?: {}", pair) }
            }
        }
        return Err(());
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
                    _ => unreachable!("{:?}", pair),
                },
            |lhs: Result<Expression, ()>, op: Pair<Rule>, rhs: Result<Expression, ()>|
                {
                    let l = lhs.unwrap();
                    let r = rhs.unwrap();
                    println!("Climb {:?} - {:?} - {:?}", l, op.as_rule(), r);
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