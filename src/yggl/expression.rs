use crate::yggl::environment::{Environment, Variable};
use crate::yggl::data::{Constant, DataType};
use std::fmt;

/// Expressions represent portions of code that evaluate to values
#[derive(Clone)]
#[allow(dead_code)]
pub enum Expression {
    Variable(Variable),
    Constant(Constant),
    UnaryOperation(Box<Expression>, UnaryOperation),
    BinaryOperation(Box<Expression>, BinaryOperation, Box<Expression>),
}

impl Expression {
    pub fn eval(&self, environment: &Environment) -> Constant {
        match self {
            &Expression::Constant(ref c) => c.clone(),
            &Expression::Variable(ref var) => {
                match var.eval(environment) {
                    Ok(c) => c,
                    Err(_) => { panic!("sdfsdf") }
                }
            }
            &Expression::UnaryOperation(ref exp, ref op) => {
                match op {
                    UnaryOperation::Inc => exp.eval(environment) + Constant::Int(1),
                    UnaryOperation::Dec => exp.eval(environment) - Constant::Int(1)
                }
            }
            &Expression::BinaryOperation(ref lexp, ref op, ref rexp) => {
                match op {
                    BinaryOperation::Sum => lexp.eval(environment) + rexp.eval(environment),
                    BinaryOperation::Sub => lexp.eval(environment) - rexp.eval(environment),
                    BinaryOperation::Mul => lexp.eval(environment) * rexp.eval(environment),
                    BinaryOperation::Div => lexp.eval(environment) / rexp.eval(environment)
                }
            }
        }
    }

    pub fn data_type(&self) -> Option<DataType> {
        match self {
            &Expression::Constant(ref c) => Some(c.data_type()),
            &Expression::UnaryOperation(ref exp, _) => exp.data_type(),
            &Expression::BinaryOperation(ref exp, _, _) => exp.data_type(),
            _ => { None }
        }
    }

    pub fn transpile(&self, env: &mut Environment) -> String {
        match self {
            &Expression::Constant(ref c) => format!("{}", c),
            &Expression::UnaryOperation(ref exp, ref op) => {
                format!("({}{})", op, exp.transpile(env))
            }
            &Expression::BinaryOperation(ref lhs, ref op, ref rhs) => {
                format!("({} {} {})", lhs.transpile(env), op, rhs.transpile(env))
            }
            _ => { "".to_string() }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            &Expression::Variable(id) => write!(f, "{}", id),
            &Expression::Constant(v) => write!(f, "{}", v),
            &Expression::UnaryOperation(a, op) => write!(f, "{}{}", a, op),
            &Expression::BinaryOperation(l, op, r) => write!(f, "({}{}{})", l, op, r),
        }
    }
}


#[derive(Clone)]
#[allow(dead_code)]
pub enum UnaryOperation { Inc, Dec }

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOperation::Inc => "++",
            &UnaryOperation::Dec => "--",
        })
    }
}

#[derive(Clone)]
#[allow(dead_code)]
pub enum BinaryOperation { Sum, Sub, Mul, Div }

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperation::Sum => "+",
            &BinaryOperation::Sub => "-",
            &BinaryOperation::Mul => "*",
            &BinaryOperation::Div => "/",
        })
    }
}