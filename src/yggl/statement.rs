use std::string::ToString;
use crate::yggl::data::{DataType, Evaluable};
use crate::yggl::environment::Environment;
use crate::yggl::expression::Expression;
use std::fmt;
use crate::yggl::language::Program;

/// Statements are standalone instructions.
/// They are meaningful by themselves, as long as the current environment is able to handle them.
#[allow(dead_code)]
pub enum Statement {
    Assignment(String, Expression),
    FunctionDef(String, usize),
    Call(usize, Vec<Expression>),
    Print(Vec<Expression>),
    Return(Box<Evaluable>),
}

#[allow(dead_code)]
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

    pub fn transpile(&self, program: &Program, env: &Environment) -> String {
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
                        ;
                    } else {
                        panic!("Print of non-valued expression");
                    }
                }

                format!("printf(\"{}\"{});", format_string, expressions_string)
            }
            &Statement::FunctionDef(ref identifier, function_id) => {
                let function = program.get_function(function_id);
                function.transpile(program, identifier.as_str())
            }
            _ => { "".to_string() }
        }
    }
}

impl<'a> fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Assignment(id, exp) =>
                write!(f, "Assign {}->{}", exp, id),
            Statement::FunctionDef(id, _) => write!(f, "Assign Fun->{}", id),
            Statement::Call(_, _) => write!(f, "FunCall"),
            Statement::Print(_) => write!(f, "A print statement"),
            Statement::Return(_) => write!(f, "A return statement"),
        }
    }
}