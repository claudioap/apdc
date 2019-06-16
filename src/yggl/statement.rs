use std::fmt;
use std::rc::Rc;
use crate::yggl::data::DataType;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::expression::Expression;
use crate::yggl::function::{FunctionCall, Function};
use crate::yggl::flow::{Conditional, Cycle};
use crate::yggl::structure::{StructDef, StructDecl, Attribute};
use crate::yggl::timer::Timer;
use crate::yggl::networking::Address;

/// Statements are standalone instructions.
/// They are meaningful by themselves, as long as the current environment is able to handle them.

pub enum Statement {
    // TODO String -> Rc<Variable>, Constants?!?
    Declaration(Rc<Variable>),
    Allocation(Rc<Variable>, Rc<StructDecl>),
    Assignment(Rc<Variable>, Expression),
    StructDecl(Rc<StructDecl>),
    StructDef(Rc<Variable>, Rc<StructDef>),
    FunctionDef(Rc<Function>),
    Call(FunctionCall),
    Conditional(Conditional),
    Cycle(Cycle),
    Print(Vec<Expression>),
    Return(Expression),
    AttributeAssignment(Rc<Variable>, Rc<Attribute>, Expression),
    Setup(Rc<Variable>, Timer),
    Send(Rc<Variable>, Address, Vec<Expression>),
    Notify(String, Rc<StructDef>),
}

impl Statement {
    pub fn run(&self, env: &mut Environment) {
        match self {
            Statement::Assignment(var, exp) => {
                let evaluation = exp.eval(env);
                var.set_content(evaluation);
            }
            Statement::Print(expressions) => {
                for expression in expressions.iter() {
                    println!("{}", expression.eval(env));
                }
            }
            _ => {}
        }
    }

    pub fn transpile(&self, env: &Environment) -> String {
        match self {
            Statement::Declaration(var) => {
                match var.data_type().unwrap() {
                    DataType::Struct(_) => {
                        format!("{}* {};", var.data_type().unwrap().transpile(), var.get_identifier())
                    }
                    _ => {
                        format!("{} {};", var.data_type().unwrap().transpile(), var.get_identifier())
                    }
                }
            }
            Statement::Assignment(identifier, exp) => {
                format!("{} = {};", identifier.get_identifier(), exp.transpile())
            }
            Statement::Print(expressions) => {
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
                            DataType::Struct(_) => panic!("Attempted to print a struct..."),
                            DataType::Reference(_) => panic!("Attempted to print a reference..."),
                        }
                        expressions_string.push_str(",");
                        expressions_string.push_str(expression.transpile().as_str());
                    } else {
                        panic!("Print of non-valued expression: {}", expression);
                    }
                }

                format!("printf(\"{}\"{});", format_string, expressions_string)
            }
            Statement::Conditional(conditional) => conditional.transpile(env),
            Statement::Cycle(cycle) => cycle.transpile(env),
            Statement::StructDef(var, definition) => {
                format!("struct {}* {};", definition.get_declaration().get_name(), var.get_identifier())
            }
            Statement::Allocation(var, _) => {
                format!("{} = malloc(sizeof({}));", var.get_identifier(), var.data_type().unwrap().transpile())
            }
            Statement::Return(expression) => {
                format!("return {};", expression.transpile())
            }
            Statement::StructDecl(_) | Statement::FunctionDef(_) => {
                "".to_string()
            }
            Statement::Setup(_, _) => {
                "TODO SETUP".to_string()
            }
            Statement::Send(_, _, _) => {
                "TODO SEND".to_string()
            }
            Statement::Notify(_, _) => {
                "TODO NOTIFY".to_string()
            }
            _ => { unimplemented!() }
        }
    }
}

impl fmt::Display for Statement {
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
            Statement::StructDecl(def) => write!(f, "{}", def),
            _ => write!(f, "TODO")
        }
    }
}