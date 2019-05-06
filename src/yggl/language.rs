use std::collections::{HashMap, LinkedList, HashSet};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::function::Function;
use crate::yggl::data::Constant;
use std::fmt;


/// A program is the AST root.
/// A new environment starts with a program.
/// Static variables that are declared outside the scope of any function,
/// are attached to the program.
/// Within a program there is a list of root statements.
#[allow(dead_code)]
pub struct Program {
    environment: Environment,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
    functions: Vec<Function>,
    includes: HashSet<Include>,
}

#[allow(dead_code)]
impl Program {
    pub fn new() -> Program {
        Program {
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements: LinkedList::new(),
            functions: vec!(),
            includes: HashSet::new(),
        }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push_back(statement);
    }

    pub fn run(&mut self) {
        for statement in &self.statements {
            statement.run(&mut self.environment);
        }
    }

    pub fn transpile(&mut self) -> String {
        let mut output = String::new();
        output.reserve(1024 * 1024); // Reserve 1MB upfront as the new program buffer
        output.push_str("// Includes\n");
        for include in &self.includes {
            output.push_str(format!("{}\n", include).as_str());
        }
        output.push_str("\n// Functions\n");
        for function in &self.functions {
            output.push_str(function.transpile(&self).as_str());
            output.push('\n');
        }
        output.push_str("int main(){\n");
        for statement in &self.statements {
            output.push_str("    ");
            output.push_str(statement.transpile(&self, &self.environment).as_str());
            output.push_str("\n");
        }
        output.push_str("    return 0;\n}");
        output
    }

    pub fn add_function(&mut self, function: Function) -> usize {
        self.functions.push(function);
        self.functions.len() - 1
    }

    pub fn get_env(&self) -> &Environment {
        &self.environment
    }

    pub fn get_function(&self, identifier: usize) -> &Function {
        &self.functions[identifier]
    }

    pub fn call_function(&mut self, identifier: usize) -> Option<Constant> {
        self.functions[identifier].call()
    }

    pub fn require_include(&mut self, include: Include) {
        self.includes.insert(include);
    }
}

#[derive(PartialOrd, PartialEq, Eq, Hash, Debug)]
pub struct Include {
    name: String,
    std: bool,
    path: Option<String>,
}

impl Include {
    pub fn new(name: String, std: bool, path: Option<String>) -> Include {
        Include { name, std, path }
    }
}

impl fmt::Display for Include {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.std {
            write!(f, "#include <{}>", self.name)
        } else if let Some(path) = &self.path {
            write!(f, "#include \"{}\\{}\"", path, self.name)
        } else {
            write!(f, "#include \"{}\"", self.name)
        }
    }
}