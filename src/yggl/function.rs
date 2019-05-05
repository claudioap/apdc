use std::collections::{HashMap, LinkedList};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::language::Program;
use crate::yggl::data::DataType;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
#[allow(dead_code)]
pub struct Function {
    parameters: Vec<Variable>,
    environment: Environment,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
    return_type: Option<DataType>,
}

impl Function {
    pub fn new(parameters: Vec<Variable>, statements: LinkedList<Statement>) -> Function {
        Function {
            parameters,
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements,
            return_type: None,
        }
    }

    pub fn transpile(&self, program: &Program, identifier: &str) -> String {
        let rtype = match &self.return_type {
            Some(dtype) => dtype.transpile(),
            _ => "void"
        };
        let mut result = format!("\n{} {}((", rtype, identifier);
        for parameter in &self.parameters {
            let dtype = parameter.get_type().expect("Parameter type unknown");
            let identifier = parameter.get_identifier();
            result.push_str(dtype.transpile());
            result.push_str(" ");
            result.push_str(identifier);
            result.push_str(",");
        }
        result.pop();// Remove last comma
        result.push_str("){\n");
        for statement in &self.statements {
            result.push_str(format!(
                "    {}\n",
                statement.transpile(program, &self.environment)).as_str());
        }
        result.push_str("}\n");
        result
    }
}