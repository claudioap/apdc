use std::string::ToString;
use std::fmt;
use std::rc::Rc;
use crate::yggl::data::DataType;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::expression::Expression;
use crate::yggl::function::{FunctionCall, Function};
use crate::yggl::flow::{Conditional, Cycle};

/// Statements are standalone instructions.
/// They are meaningful by themselves, as long as the current environment is able to handle them.
#[allow(dead_code)]
pub enum Statement {
    Assignment(String, Expression),
    FunctionDef(Rc<Function>),
    Call(FunctionCall),
    Conditional(Conditional),
    Cycle(Cycle),
    Print(Vec<Expression>),
    Return(Rc<Variable>),
}

impl Statement {
    pub fn run(&self, env: &mut Environment) {
        match self {
            &Statement::Assignment(ref identifier, ref exp) => {
                let evaluation = exp.eval(env);
                let _ = env.define(identifier.as_str(), evaluation);
            }
            &Statement::Print(ref expressions) => {
                for expression in expressions.iter() {
                    println!("{}", expression.eval(env));
                }
            }
            _ => {}
        }
    }

    pub fn transpile(&self, env: &Environment) -> String {
        match self {
            &Statement::Assignment(ref identifier, ref exp) => {
                format!("{} = {};", identifier, exp.transpile(env))
            }
            &Statement::Print(ref expressions) => {
                let mut format_string = String::new();
                format_string.reserve(expressions.len() * 4);
                let mut expressions_string = String::new();
                for expression in expressions.iter() {
                    if let Some(dtype) = expression.data_type() {
                        match dtype {
                            DataType::Bool =>
                                format_string.push_str("%i\\n"),
                            DataType::Int =>
                                format_string.push_str("%i\\n"),
                            DataType::Float =>
                                format_string.push_str("%f\\n"),
                            DataType::Char =>
                                format_string.push_str("%c\\n"),
                            DataType::String =>
                                format_string.push_str("%s\\n"),
                            DataType::Function => {
                                // TODO maybe call if function takes no parameters
                                panic!("Attempted to print a function...");
                            }
                        }
                        expressions_string.push_str(",");
                        expressions_string.push_str(expression.transpile(env).as_str());
                    } else {
                        panic!("Print of non-valued expression");
                    }
                }

                format!("printf(\"{}\"{});", format_string, expressions_string)
            }
            &Statement::Conditional(ref conditional) => conditional.transpile(env),
            &Statement::Cycle(ref cycle) => cycle.transpile(env),
            _ => { "".to_string() }
        }
    }
}

impl<'a> fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Assignment(id, exp) =>
                write!(f, "Assign {}->{}", exp, id),
            Statement::FunctionDef(_) => write!(f, "Function definition"),
            Statement::Call(_) => write!(f, "FunCall"),
            Statement::Print(_) => write!(f, "A print statement"),
            Statement::Return(_) => write!(f, "A return statement"),
            Statement::Conditional(_) => write!(f, "An if clause"),
            Statement::Cycle(_) => write!(f, "A cycle"),
        }
    }
}