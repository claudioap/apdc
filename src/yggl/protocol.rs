use std::fmt;
use std::rc::Rc;
use std::collections::HashSet;
use crate::yggl::structure::StructDecl;
use crate::yggl::function::Function;
use crate::yggl::environment::{Variable, Environment};
use crate::yggl::foreign::{ProtoAddMLoopCall, ProtoCreateCall, ForeignFunctionCall};
use crate::yggl::statement::Statement;
use crate::parser::CompilationError;
use crate::yggl::expression::Expression;

pub enum YggType {
    Request,
    Reply,
    Message,
    Timer,
    Notification,
}

impl fmt::Display for YggType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            YggType::Request => "request",
            YggType::Reply => "reply",
            YggType::Message => "message",
            YggType::Timer => "timer",
            YggType::Notification => "notification"
        })
    }
}

pub enum Interface {
    Consumer(YggType, Rc<Variable>, u32),
    Producer(YggType, Rc<Variable>, u32, u32),
}

pub struct Handler {
    identifier: String,
    ytype: YggType,
    function: Rc<Function>,
}

impl Handler {
    pub fn new(identifier: String, ytype: YggType, function: Rc<Function>) -> Handler {
        Handler { identifier, ytype, function }
    }
}

pub struct ProtocolDef {
    id: u32,
    name: String,
    state: Rc<StructDecl>,
    init: Rc<Function>,
    main_loop: Option<Rc<Function>>,
    interfaces: Vec<Interface>,
    handlers: Vec<Handler>,
}

impl ProtocolDef {
    pub fn new(id: u32, name: String, state: Rc<StructDecl>, init: Rc<Function>) -> ProtocolDef {
        ProtocolDef { id, name, state, init, main_loop: None, interfaces: vec![], handlers: vec![] }
    }

    pub fn add_interfaces(&mut self, mut interfaces: Vec<Interface>) {
        self.interfaces.append(&mut interfaces);
    }
    pub fn add_handlers(&mut self, mut handlers: Vec<Handler>) {
        self.handlers.append(&mut handlers);
    }

    pub fn add_main_loop(&mut self, main_loop: Rc<Function>) {
        self.main_loop = Some(main_loop)
    }

    pub fn build_init_function(&self, env: &mut Environment) -> Result<Function, CompilationError> {
        let aux = env.declare_aux();
        let proto_creation_call = ProtoCreateCall::new(Rc::clone(&self.state));
        aux.set_type(proto_creation_call.return_type().unwrap());
        let mut statements = vec![
            Statement::Assignment(
                Rc::clone(&aux),
                Expression::Foreign(Box::new(proto_creation_call)),
            )
        ];
        if let Some(main_loop) = &self.main_loop {
            statements.push(
                Statement::ForeignCall(Box::new(
                    ProtoAddMLoopCall::new(
                        Rc::clone(&aux),
                        Rc::clone(main_loop)))));
        }
        statements.push(Statement::Return(Expression::Variable(Rc::clone(&aux))));
        let func_env = Environment::new();
        let mut func = Function::new(
            func_env,
            format!("{}_init", self.name),
            vec![],
            statements,
        )?;
        func.set_exported();
        Ok(func)
    }
}

/// A program is the AST root.
/// A new environment starts with a program.
/// Static variables that are declared outside the scope of any function,
/// are attached to the program.
/// Within a program there is a list of root statements.
#[allow(dead_code)]
pub struct Protocol {
    environment: Environment,
    // statements: Vec<Statement>,
    includes: HashSet<Include>,
    definition: Option<ProtocolDef>,
    compiled: bool,
}

#[allow(dead_code)]
impl Protocol {
    pub fn new() -> Protocol {
        Protocol {
            environment: Environment::new(),
            includes: HashSet::new(),
            definition: None,
            compiled: false,
        }
    }

    pub fn transpile(&mut self) -> String {
        let mut output = String::new();
        output.reserve(1024 * 1024); // Reserve 1MB upfront as the new program buffer
        output.push_str("// Includes\n");
        for include in &self.includes {
            output.push_str(format!("{}\n", include).as_str());
        }
        output.push_str("\n// Structures\n");
        for struct_decl in self.environment.get_struct_decls() {
            if !struct_decl.is_exported() {
                output.push_str(struct_decl.transpile().as_str());
                output.push('\n');
            }
        }
        output.push_str("\n// Functions\n");
        for function in &self.environment.get_functions() {
            if !function.is_exported() {
                output.push_str(function.transpile_signature().as_str());
                output.push('\n');
            }
        }
        for function in &self.environment.get_functions() {
            output.push_str(function.transpile().as_str());
            output.push('\n');
        }
        output
    }

    /// Sets protocol as finished and calculates the remaining details
    pub fn compile(&mut self) -> Result<(), CompilationError> {
        if let Some(definition) = &self.definition {
            let init = definition.build_init_function(&mut self.environment)?;
            self.environment.add_function(init);
            self.compiled = true;
        } else {
            panic!();
        }
        Ok(())
    }

    pub fn transpile_header(&mut self) -> String {
        let mut output = String::new();
        output.reserve(10 * 1024); // Reserve 10 KB upfront as the new program header buffer
        output.push_str("// Defines\n");
        for define in self.environment.get_defines() {
            output.push_str(
                format!("#define {} {}\n",
                        define.get_identifier(),
                        define.value()
                ).as_str());
        }
        output.push_str("\n// Structures\n");
        for struct_decl in self.environment.get_struct_decls() {
            if struct_decl.is_exported() {
                output.push_str(struct_decl.transpile().as_str());
                output.push('\n');
            }
        }
        output.push_str("\n// Functions\n");
        for function in &self.environment.get_functions() {
            if function.is_exported() {
                output.push_str(function.transpile_signature().as_str());
                output.push('\n');
            }
        }
        output
    }

    pub fn get_env(&self) -> &Environment {
        &self.environment
    }

    pub fn get_env_mut(&mut self) -> &mut Environment {
        if self.compiled {
            panic!("Cannot mutate compiled protocol.")
        }
        &mut self.environment
    }

    pub fn require_include(&mut self, include: Include) {
        self.includes.insert(include);
    }

    pub fn set_definition(&mut self, definition: ProtocolDef) {
        if self.definition.is_some() {
            panic!("Attempted to change the protocol definition.");
        }
        self.includes.insert(
            Include::new(format!("{}.h", definition.name), false, None));
        self.definition = Some(definition);
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