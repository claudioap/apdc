use std::collections::HashSet;
use std::fmt;
use crate::yggl::environment::Environment;
use crate::yggl::statement::Statement;
use std::rc::Rc;


/// A program is the AST root.
/// A new environment starts with a program.
/// Static variables that are declared outside the scope of any function,
/// are attached to the program.
/// Within a program there is a list of root statements.
#[allow(dead_code)]
pub struct Program {
    environment: Environment,
    statements: Vec<Statement>,
    includes: HashSet<Include>,
}

#[allow(dead_code)]
impl Program {
    pub fn new() -> Program {
        Program {
            environment: Environment::new(),
            statements: vec![],
            includes: HashSet::new(),
        }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn annotate(&mut self) {
        loop {
            for statement in &self.statements {
                match statement {
                    Statement::Assignment(ref variable, ref exp) => {
                        let dtype = exp.data_type();
                        if dtype.is_none() {
                            continue;
                        }
                        if !variable.has_type() {
                            variable.set_type(dtype.unwrap());
                        }
                    }
                    _ => {}
                }
            }
            break;
        }

        let mut index = 0;
        let mut declarations = vec![];
        for statement in &mut self.statements {
            if let Statement::Assignment(ref var, _) = statement {
                if !var.is_declared() {
                    declarations.push((index, Statement::Declaration(Rc::clone(var))));
                    var.set_declared();
                }
            }
            index += 1;
        }

        let mut offset = 0;
        for declaration in declarations {
            self.statements.insert(declaration.0 + offset, declaration.1);
            offset += 1;
        }
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
        for include in self.environment.get_includes() {
            output.push_str(format!("{}\n", include).as_str());
        }
        output.push_str("\n// Functions\n");
        for function in &self.environment.get_functions() {
            output.push_str(function.transpile().as_str());
            output.push('\n');
        }
        output.push_str("\n// Structures\n");
        for struct_def in self.environment.get_struct_defs() {
            output.push_str(struct_def.transpile().as_str());
            output.push('\n');
        }
        output.push_str("int main(){\n");
        for statement in &self.statements {
            let transpilation = statement.transpile(&self.environment).replace("\n", "\n    ");
            if transpilation == "" {
                continue;
            }
            output.push_str("    ");
            output.push_str(transpilation.as_str());
            output.push_str("\n");
        }
        output.push_str("    return 0;\n}");
        output
    }

    pub fn get_env(&self) -> &Environment {
        &self.environment
    }

    pub fn get_env_mut(&mut self) -> &mut Environment {
        &mut self.environment
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