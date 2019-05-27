use std::string::ToString;
use std::fmt;
use std::rc::Rc;
use crate::yggl::data::{DataType, Evaluable};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::expression::Expression;
use crate::yggl::function::{FunctionCall, Function};
use crate::yggl::flow::{Conditional, Cycle};
use crate::yggl::structure::{StructDef, StructDecl, Attribute};

/// Statements are standalone instructions.
/// They are meaningful by themselves, as long as the current environment is able to handle them.
#[allow(dead_code)]
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
    Return(Rc<Variable>),
    AttributeAssignment(Rc<Variable>, Rc<Attribute>, Expression),
//    ProtocolDef(),
//    TimerInit(),
//    TimerAction(),
//    MessageInit(),
//    MessageAttribute(),
}

impl Statement {
    pub fn run(&self, env: &mut Environment) {
        match self {
            &Statement::Assignment(ref var, ref exp) => {
                let evaluation = exp.eval(env);
                let _ = env.define(var.get_identifier(), evaluation);
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
            &Statement::Declaration(ref var) => {
                match var.get_type().unwrap() {
                    DataType::Struct(_) => {
                        format!("{}* {};", var.get_type().unwrap().transpile(), var.get_identifier())
                    }
                    _ => {
                        format!("{} {};", var.get_type().unwrap().transpile(), var.get_identifier())
                    }
                }
            }
            &Statement::Assignment(ref identifier, ref exp) => {
                format!("{} = {};", identifier.get_identifier(), exp.transpile(env))
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
                            DataType::Struct(_) => panic!("Attempted to print a struct..."),
                            DataType::Reference(_) => panic!("Attempted to print a reference..."),
                        }
                        expressions_string.push_str(",");
                        expressions_string.push_str(expression.transpile(env).as_str());
                    } else {
                        panic!("Print of non-valued expression: {}", expression);
                    }
                }

                format!("printf(\"{}\"{});", format_string, expressions_string)
            }
            &Statement::Conditional(ref conditional) => conditional.transpile(env),
            &Statement::Cycle(ref cycle) => cycle.transpile(env),
            &Statement::StructDef(ref var, ref definition) => {
                format!("struct {}* {};", definition.get_declaration().get_name(), var.get_identifier())
            }
            &Statement::Allocation(ref var, _) => {
                format!("{} = malloc(sizeof({}));", var.get_identifier(), var.data_type().unwrap().transpile())
            }
            _ => { "".to_string() }
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