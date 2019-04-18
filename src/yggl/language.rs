use std::collections::{HashMap, LinkedList};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::function::Function;


/// A program is the AST root.
/// A new environment starts with a program.
/// Static variables that are declared outside the scope of any function,
/// are attached to the program.
/// Within a program there is a list of root statements.
#[allow(dead_code)]
pub struct Program<'a> {
    environment: Environment<'a>,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement<'a>>,
    functions: Vec<Function<'a>>,
}

#[allow(dead_code)]
impl<'a> Program<'a> {
    pub fn new() -> Program<'a> {
        Program {
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements: LinkedList::new(),
            functions: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, statement: Statement<'a>) {
        self.statements.push_back(statement);
    }

    pub fn run(&mut self) {
        for statement in &self.statements {
            statement.run(&mut self.environment);
        }
    }

    pub fn transpile(&mut self) -> String {
        let includes = "#include <stdio.h>\n";
        let mut output = String::new();
        output.reserve(10000);
        output.push_str(includes);
        for statement in &self.statements {
            output.push_str(statement.transpile(&mut self.environment).as_str());
        }
        output
    }
}


