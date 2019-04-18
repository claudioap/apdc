use std::collections::{HashMap, LinkedList};
use crate::yggl::expression::Expression;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
#[derive(Clone)]
pub struct Function<'a> {
    parameters: Vec<Expression>,
    environment: Environment<'a>,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement<'a>>,
}

