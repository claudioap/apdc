use crate::yggl::environment::{Environment, Variable};
use crate::yggl::data::{Constant, DataType};
use std::{fmt, ops};

/// Expressions represent portions of code that evaluate to values
#[derive(Clone, Debug)]
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
                    BinaryOperation::Div => lexp.eval(environment) / rexp.eval(environment),
                    BinaryOperation::Pow => lexp.eval(environment).pow(&rexp.eval(environment))
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
            &Expression::Variable(ref v) => format!("{}", v.id),
            &Expression::UnaryOperation(ref exp, ref op) => {
                format!("({}{})", op, exp.transpile(env))
            }
            &Expression::BinaryOperation(ref lhs, ref op, ref rhs) => {
                format!("({} {} {})", lhs.transpile(env), op, rhs.transpile(env))
            }
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

impl ops::Add<Expression> for Expression {
    type Output = Expression;

    fn add(self, r: Expression) -> Expression {
        use Expression::Constant;

        match self {
            Constant(c1) => match r {
                Constant(c2) => Constant(c1 + c2),
                _ => Expression::BinaryOperation(
                    Box::new(Constant(c1)),
                    BinaryOperation::Sum,
                    Box::new(r)),
            },
            _ => Expression::BinaryOperation(
                Box::new(self),
                BinaryOperation::Sum,
                Box::new(r)),
        }
    }
}

impl ops::Sub<Expression> for Expression {
    type Output = Expression;

    fn sub(self, r: Expression) -> Expression {
        use Expression::Constant;

        match self {
            Constant(c1) => match r {
                Constant(c2) => Constant(c1 - c2),
                _ => Expression::BinaryOperation(
                    Box::new(Constant(c1)),
                    BinaryOperation::Sub,
                    Box::new(r)),
            },
            _ => Expression::BinaryOperation(
                Box::new(self),
                BinaryOperation::Sub,
                Box::new(r)),
        }
    }
}

impl ops::Mul<Expression> for Expression {
    type Output = Expression;

    fn mul(self, r: Expression) -> Expression {
        use Expression::Constant;

        match self {
            Constant(c1) => match r {
                Constant(c2) => Constant(c1 * c2),
                _ => Expression::BinaryOperation(
                    Box::new(Constant(c1)),
                    BinaryOperation::Mul,
                    Box::new(r)),
            },
            _ => Expression::BinaryOperation(
                Box::new(self),
                BinaryOperation::Mul,
                Box::new(r)),
        }
    }
}

impl ops::Div<Expression> for Expression {
    type Output = Expression;

    fn div(self, r: Expression) -> Expression {
        use Expression::Constant;

        match self {
            Constant(c1) => match r {
                Constant(c2) => Constant(c1 / c2),
                _ => Expression::BinaryOperation(
                    Box::new(Constant(c1)),
                    BinaryOperation::Div,
                    Box::new(r)),
            },
            _ => Expression::BinaryOperation(
                Box::new(self),
                BinaryOperation::Div,
                Box::new(r)),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum BinaryOperation { Sum, Sub, Mul, Div, Pow }

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperation::Sum => "+",
            &BinaryOperation::Sub => "-",
            &BinaryOperation::Mul => "*",
            &BinaryOperation::Div => "/",
            &BinaryOperation::Pow => "^",
        })
    }
}