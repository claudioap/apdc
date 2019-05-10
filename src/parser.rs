use pest::Parser;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::error::Error;
use pest::error::LineColLocation::*;
use pest::prec_climber::*;
use crate::yggl::data::Constant;
use crate::yggl::expression::{Expression, BinaryOperation};
use crate::yggl::statement::Statement;
use crate::yggl::language::{Program, Include};
use crate::yggl::environment::Variable;
use crate::yggl::function::*;
use std::collections::linked_list::LinkedList;
use std::fmt;
use crate::yggl::flow::Conditional;

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
    pub fn new(line: u32, col: u32, code: String, cause: String) -> CompilationError {
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

impl Program {
    pub fn from(pairs: Pairs<Rule>) -> Result<Program, CompilationError> {
        let mut program = Program::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    let statement =
                        Statement::from(pair, &mut program)?;
                    program.add_statement(statement);
                }
                Rule::EOI => {}
                _ => unimplemented!()
            }
        }
        Ok(program)
    }
}

impl Statement {
    pub fn from(pair: Pair<Rule>, program: &mut Program) -> Result<Statement, CompilationError> {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::atribution => {
                let mut inner_rules = pair.into_inner();
                let mut pair = inner_rules.next().unwrap();
                let identifier = pair.as_str().to_string();
                pair = inner_rules.next().unwrap();
                match pair.as_rule() {
                    Rule::expression => {
                        let expression = Expression::from(pair)?;
                        return Ok(Statement::Assignment(identifier, expression));
                    }
                    Rule::function => {
                        let function = Function::from(pair, program, identifier)?;
                        let function_index = program.add_function(function);
                        return Ok(Statement::FunctionDef(function_index));
                    }
                    _ => unreachable!()
                }
            }
            Rule::function_call => {
                let call = Function::parse_call(pair);
                if let Ok(Statement::Print(_)) = call {
                    program.require_include(
                        Include::new("stdio.h".to_string(), true, None))
                }
                call
            }
            Rule::conditional_if => {
                let conditional = Conditional::from(pair, program)?;
                Ok(Statement::Conditional(conditional))
            }
            _ => unreachable!()
        }
    }
}

/// Define the precedence climber for an Expression.
/// This tells a parser which operations to parse first in an unparsed expression.
lazy_static! {
    static ref BINOP_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(or, Left),
            Operator::new(and, Left),
            Operator::new(eq, Left) | Operator::new(neq, Left),
            Operator::new(le, Left) | Operator::new(gr, Left) |
                Operator::new(leq, Left) | Operator::new(geq, Left),
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left)| Operator::new(rem, Left),
            Operator::new(pow, Right)
        ])
    };
}

impl Expression {
    pub fn from(pair: Pair<Rule>) -> Result<Expression, CompilationError> {
        let pairs = pair.into_inner();
        BINOP_CLIMBER.climb(
            pairs,
            |pair: Pair<Rule>|
                match pair.as_rule() {
                    Rule::expression => Expression::from(pair),
                    Rule::number => //TODO Trait std::str::FromStr
                        Ok(Expression::Constant(Constant::parse_number(pair.as_str()))),
                    Rule::identifier => {
                        Ok(Expression::Variable(pair.as_str().to_string()))
                    }
                    _ => unreachable!("\n{:?}\n", pair),
                },
            |lhs: Result<Expression, CompilationError>, op: Pair<Rule>, rhs: Result<Expression, CompilationError>|
                {
                    let l = lhs?;
                    let r = rhs?;
                    match op.as_rule() {
                        Rule::add => Ok(l + r),
                        Rule::sub => Ok(l - r),
                        Rule::mul => Ok(l * r),
                        Rule::div => Ok(l / r),
                        Rule::pow => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Pow,
                            Box::new(r))),
                        Rule::eq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Eq,
                            Box::new(r))),
                        Rule::neq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Neq,
                            Box::new(r))),
                        Rule::gr => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Gr,
                            Box::new(r))),
                        Rule::geq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Geq,
                            Box::new(r))),
                        Rule::le => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Le,
                            Box::new(r))),
                        Rule::leq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Leq,
                            Box::new(r))),
                        Rule::and => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::And,
                            Box::new(r))),
                        Rule::or => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Or,
                            Box::new(r))),
                        _ => unreachable!(),
                    }
                },
        )
    }
}

impl Conditional {
    pub fn from(pair: Pair<Rule>, program: &mut Program)
                -> Result<Conditional, CompilationError> {
        let mut pairs = pair.into_inner();
        let mut condition_pair = pairs.next().unwrap();
        let mut condition = Expression::from(condition_pair)?;
        let mut body = pairs.next().unwrap();
        let mut statements: Vec<Statement> = vec!();
        for statement_pair in body.into_inner() {
            let statement = Statement::from(statement_pair, program)?;
            statements.push(statement);
        }
        let mut conditional = Conditional::new(Some(condition), statements);
        loop {
            let next = pairs.next();
            if let Some(pair) = next {
                match pair.as_rule() {
                    Rule::conditional_elif => {
                        pairs = pair.into_inner();
                        condition_pair = pairs.next().unwrap();
                        condition = Expression::from(condition_pair)?;
                        body = pairs.next().unwrap();
                        let mut statements: Vec<Statement> = vec!();
                        for statement_pair in body.into_inner() {
                            let statement = Statement::from(statement_pair, program)?;
                            statements.push(statement);
                        }
                        conditional.add_sibling(Some(condition), statements);
                    }
                    Rule::conditional_else => {
                        pairs = pair.into_inner();
                        body = pairs.next().unwrap();
                        let mut statements: Vec<Statement> = vec!();
                        for statement_pair in body.into_inner() {
                            let statement = Statement::from(statement_pair, program)?;
                            statements.push(statement);
                        }
                        conditional.add_sibling(None, statements);
                    }
                    _ => unreachable!()
                }
            } else {
                break;
            }
        }

        Ok(conditional)
    }
}

impl Function {
    pub fn from(pair: Pair<Rule>, program: &mut Program, name: String)
                -> Result<Function, CompilationError> {
        let parameters: Option<Vec<Variable>> = Option::None;
        let mut statements: LinkedList<Statement> = LinkedList::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::parameters => {
                    println!("Found a parameter list");
                    Function::read_parameters(pair);
                }
                Rule::statement => {
                    println!("Found a statement {:?}", pair);
                    let statement = Statement::from(pair, program)?;
                    statements.push_back(statement);
                }
                _ => unreachable!()
            }
        }
        if statements.is_empty() {
            return Err(
                CompilationError::new(
                    0,
                    0,
                    "".to_string(),
                    "Function definition without statements".to_string()));
        }
        if let Some(parameters_vec) = parameters {
            Function::new(name, parameters_vec, statements)
        } else {
            Function::new(name, vec!(), statements)
        }
    }

    fn read_parameters(pair: Pair<Rule>) -> Vec<Variable> {
        println!("{:?}", pair);
        vec![]
    }


    fn read_arguments(pair: Pair<Rule>) -> Vec<Expression> {
        println!("{:?}", pair);
        vec![]
    }

    fn parse_call(pair: Pair<Rule>) -> Result<Statement, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let identifier = inner_rules.next().unwrap().as_str();
        let arguments = Function::read_arguments(inner_rules.next().unwrap());
        let statement = if identifier == "print" {
            Statement::Print(arguments)
        } else {
            Statement::Call(FunctionCall::new(0, None, vec!()), arguments)
        };
        Ok(statement)
    }
}