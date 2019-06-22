use std::{fmt, ops};
use std::rc::Rc;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::data::{Constant, DataType};
use crate::yggl::structure::LocalAttribute;
use crate::yggl::function::FunctionCall;
use crate::yggl::foreign::ForeignFunctionCall;

/// Expressions represent portions of code that evaluate to values
#[allow(dead_code)]
pub enum Expression {
    Variable(Rc<Variable>),
    Constant(Constant),
    UnaryOperation(Box<Expression>, UnaryOperation),
    BinaryOperation(Box<Expression>, BinaryOperation, Box<Expression>),
    AttributeAccess(Rc<Variable>, Rc<LocalAttribute>),
    Call(FunctionCall),
    Foreign(Box<dyn ForeignFunctionCall>),
}

impl Expression {
    pub fn eval(&self, environment: &Environment) -> Constant {
        match self {
            Expression::Constant(c) => c.clone(),
            Expression::Variable(var) => {
                if let Some(c) = var.content() {
                    c
                } else {
                    panic!("Variable evaluation didn't return a constant")
                }
            }
            Expression::UnaryOperation(exp, op) =>
                match op {
                    UnaryOperation::Inc => exp.eval(environment) + Constant::Int(1),
                    UnaryOperation::Dec => exp.eval(environment) - Constant::Int(1),
                    UnaryOperation::Ref | UnaryOperation::Deref => unimplemented!()
                },
            Expression::BinaryOperation(lexp, op, rexp) => {
                match op {
                    BinaryOperation::Sum => lexp.eval(environment) + rexp.eval(environment),
                    BinaryOperation::Sub => lexp.eval(environment) - rexp.eval(environment),
                    BinaryOperation::Mul => lexp.eval(environment) * rexp.eval(environment),
                    BinaryOperation::Div => lexp.eval(environment) / rexp.eval(environment),
                    BinaryOperation::Pow => lexp.eval(environment).pow(&rexp.eval(environment)),
                    BinaryOperation::Eq | BinaryOperation::Neq | BinaryOperation::Gr |
                    BinaryOperation::Geq | BinaryOperation::Le | BinaryOperation::Leq |
                    BinaryOperation::And | BinaryOperation::Or =>
                        Constant::Bool(self.bin_eval(environment)),
                }
            }
            Expression::AttributeAccess(_, _) | Expression::Call(_) | Expression::Foreign(_) => {
                unimplemented!();
            }
        }
    }

    pub fn bin_eval(&self, environment: &Environment) -> bool {
        match self {
            Expression::Constant(c) => c.truth_value(),
            Expression::Variable(_) => self.eval(environment).truth_value(),
            Expression::UnaryOperation(exp, op) => {
                match op {
                    UnaryOperation::Inc => (exp.eval(environment) + Constant::Int(1)).truth_value(),
                    UnaryOperation::Dec => (exp.eval(environment) - Constant::Int(1)).truth_value(),
                    UnaryOperation::Ref | UnaryOperation::Deref => unimplemented!()
                }
            }
            Expression::BinaryOperation(lexp, op, rexp) => {
                match op {
                    BinaryOperation::Sum =>
                        (lexp.eval(environment) + rexp.eval(environment)).truth_value(),
                    BinaryOperation::Sub =>
                        (lexp.eval(environment) - rexp.eval(environment)).truth_value(),
                    BinaryOperation::Mul =>
                        (lexp.eval(environment) * rexp.eval(environment)).truth_value(),
                    BinaryOperation::Div =>
                        (lexp.eval(environment) / rexp.eval(environment)).truth_value(),
                    BinaryOperation::Pow =>
                        lexp.eval(environment).pow(&rexp.eval(environment)).truth_value(),
                    BinaryOperation::Eq =>
                        lexp.eval(environment) == rexp.eval(environment),
                    BinaryOperation::Neq =>
                        lexp.eval(environment) != rexp.eval(environment),
                    BinaryOperation::Gr =>
                        lexp.eval(environment) > rexp.eval(environment),
                    BinaryOperation::Geq =>
                        lexp.eval(environment) >= rexp.eval(environment),
                    BinaryOperation::Le =>
                        lexp.eval(environment) < rexp.eval(environment),
                    BinaryOperation::Leq =>
                        lexp.eval(environment) <= rexp.eval(environment),
                    BinaryOperation::And =>
                        lexp.eval(environment).truth_value() && rexp.eval(environment).truth_value(),
                    BinaryOperation::Or =>
                        lexp.eval(environment).truth_value() || rexp.eval(environment).truth_value(),
                }
            }
            _ => unimplemented!()
        }
    }

    pub fn data_type(&self) -> Option<DataType> {
        match self {
            Expression::Constant(c) => Some(c.data_type()),
            Expression::Variable(v) => v.data_type(),
            Expression::UnaryOperation(exp, op) => {
                match op {
                    UnaryOperation::Inc | UnaryOperation::Dec => exp.data_type(),
                    UnaryOperation::Ref => {
                        if let Some(dtype) = exp.data_type() {
                            Some(DataType::Reference(Box::new(dtype)))
                        } else {
                            None
                        }
                    }
                    UnaryOperation::Deref => {
                        if let Some(DataType::Reference(dtype)) = exp.data_type() {
                            Some(*dtype)
                        } else {
                            None
                        }
                    }
                }
            }
            Expression::BinaryOperation(exp, _, _) => exp.data_type(),
            Expression::AttributeAccess(_, attr) => attr.data_type(),
            Expression::Call(call) => call.data_type(),
            Expression::Foreign(call) => call.return_type(),
        }
    }

    pub fn transpile(&self) -> String {
        match self {
            Expression::Constant(c) => format!("{}", c),
            Expression::Variable(v) => format!("{}", v.get_identifier()),
            Expression::UnaryOperation(exp, op) =>
                format!("({}{})", op, exp.transpile()),
            Expression::BinaryOperation(lhs, op, rhs) =>
                format!("({} {} {})", lhs.transpile(), op, rhs.transpile()),
            Expression::AttributeAccess(var, attr) =>
                format!("{}->{}", var.get_identifier(), attr.get_name()),
            Expression::Call(call) => call.transpile(),
            Expression::Foreign(call) => call.transpile(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Expression::Variable(id) =>
                write!(f, "{}", id),
            Expression::Constant(v) =>
                write!(f, "{}", v),
            Expression::UnaryOperation(a, op) =>
                write!(f, "{}{}", op, a),
            Expression::BinaryOperation(l, op, r) =>
                write!(f, "({}{}{})", l, op, r),
            Expression::AttributeAccess(var, attr) =>
                write!(f, "{}->{}", var.get_identifier(), attr.get_name()),
            Expression::Call(call) =>
                write!(f, "{}", call.transpile()),
            Expression::Foreign(call) =>
                write!(f, "{}", call.transpile())
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
pub enum UnaryOperation { Inc, Dec, Ref, Deref }

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOperation::Inc => "++",
            UnaryOperation::Dec => "--",
            UnaryOperation::Ref => "&",
            UnaryOperation::Deref => "*",
        })
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOperation { Sum, Sub, Mul, Div, Pow, Eq, Neq, Gr, Geq, Le, Leq, And, Or }

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOperation::Sum => "+",
            BinaryOperation::Sub => "-",
            BinaryOperation::Mul => "*",
            BinaryOperation::Div => "/",
            BinaryOperation::Pow => "^",
            BinaryOperation::Eq => "==",
            BinaryOperation::Neq => "!=",
            BinaryOperation::Gr => ">",
            BinaryOperation::Geq => ">=",
            BinaryOperation::Le => "<",
            BinaryOperation::Leq => "<=",
            BinaryOperation::And => "&&",
            BinaryOperation::Or => "||",
        })
    }
}