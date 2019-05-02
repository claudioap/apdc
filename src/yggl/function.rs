use std::collections::{HashMap, LinkedList};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
#[allow(dead_code)]
pub struct Function {
    parameters: Vec<Variable>,
    environment: Environment,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
}

impl Function{
    pub fn new(parameters: Vec<Variable>, statements : LinkedList<Statement>) -> Function{
        Function {
            parameters,
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements
        }
    }
}