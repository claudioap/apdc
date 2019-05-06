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
    name: String,
    parameters: Vec<Variable>,
    environment: Environment,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
    return_type: Option<DataType>,
}

impl Function {
    pub fn new(name: String, parameters: Vec<Variable>, statements: LinkedList<Statement>) -> Function {
        let dtype = Function::determine_return(&statements);
        Function {
            name,
            parameters,
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements,
            return_type: dtype,
        }
    }

    fn determine_return(_statements: &LinkedList<Statement>) -> Option<DataType> {
        None
    }

    pub fn transpile(&self, program: &Program, identifier: &str) -> String {
        let rtype = match &self.return_type {
            Some(dtype) => dtype.transpile(),
            _ => "void"
        };
        let mut result = String::new();
        result.reserve(1024 * 10); // Reserve 10 KB to prevent further allocations
        result.push_str(format!("\n{} {}((", rtype, identifier).as_str());
        for parameter in &self.parameters {
            let dtype = parameter.get_type().expect("Parameter type unknown");
            let identifier = parameter.get_identifier();
            result.push_str(dtype.transpile());
            result.push(' ');
            result.push_str(identifier);
            result.push(',');
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