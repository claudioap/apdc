use std::collections::{HashMap, LinkedList};
use crate::yggl::data::{Constant, DataType};
use std::fmt;
use crate::yggl::statement::Statement;

/// A program has a set of environments, which hold variable data.
pub struct Environment {
    scopes: LinkedList<Scope>,
}

#[allow(dead_code)]
impl Environment {
    pub fn new() -> Environment {
        let mut scopes = LinkedList::new();
        let scope = Scope::new();
        scopes.push_back(scope);
        Environment { scopes }
    }

    pub fn push_scope(&mut self) {
        let scope = Scope::new();
        self.scopes.push_front(scope);
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Attempted to remove last scope from the environment.")
        }
        self.scopes.pop_front();
    }

    fn get_current_scope(&mut self) -> &mut Scope {
        self.scopes.front_mut().unwrap()
    }

    fn get_definition_scope_mut(&mut self, identifier: &str) -> Option<&mut Scope> {
        for scope in &mut self.scopes {
            if scope.is_defined(identifier) {
                return Some(scope);
            }
        }
        None
    }


    fn get_definition_scope(&self, identifier: &str) -> Option<&Scope> {
        for scope in &self.scopes {
            if scope.is_defined(identifier) {
                return Some(scope);
            }
        }
        None
    }

    pub fn define(&mut self, identifier: &str, constant: Constant) {
        match self.get_definition_scope_mut(identifier) {
            Some(scope) => scope.define(identifier, constant),
            None => self.get_current_scope().define(identifier, constant)
        }
    }

    pub fn eval(&self, identifier: &str) -> Option<Constant>{
        match self.get_definition_scope(identifier){
            Some(scope) => scope.eval(identifier),
            None => None
        }

    }
}


/// A scope is a slice of the current environment.
/// Scopes can be stacked on top of each other, and variable resolution is done as a LIFO.
#[derive(Clone)]
struct Scope {
    symbols: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
}

#[allow(dead_code)]
impl Scope {
    pub fn new() -> Scope {
        Scope {
            symbols: HashMap::new(),
            statements: LinkedList::new(),
        }
    }

    fn declare(&mut self, identifier: &str) {
        if self.symbols.contains_key(identifier) {
            panic!("Declared same variable twice");
        } else {
            let var = Variable::new(identifier);
            self.symbols.insert(identifier.to_string(), var);
        }
    }

    fn define(&mut self, identifier: &str, constant: Constant) {
        if let Some(mut var) = self.symbols.get_mut(identifier) {
            match &var.data_type {
                Some(dt) if dt.clone() != constant.data_type() =>
                    panic!("Assigned to variable of different data type"),
                Some(_) => var.content = Some(constant),
                None => {
                    var.data_type = Some(constant.data_type());
                    var.content = Some(constant);
                }
            }
        } else {
            let var = Variable {
                id: identifier.to_string(),
                data_type: Some(constant.data_type()),
                content: Some(constant),
            };
            self.symbols.insert(identifier.to_string(), var);
        }
    }

    pub fn is_defined(&self, identifier: &str) -> bool {
        self.symbols.contains_key(identifier)
    }

    pub fn get(&self, identifier: &str) -> Option<&Variable> {
        self.symbols.get(identifier)
    }

    pub fn eval(&self, identifier: &str) -> Option<Constant> {
        match self.symbols.get(identifier) {
            Some(var) => var.content.clone(),
            None => None
        }
    }

    pub fn get_datatype(&self, identifier: &str) -> Option<DataType> {
        let symbol = self.symbols.get(identifier);
        if let Some(var) = symbol {
            var.data_type.clone()
        } else {
            None
        }
    }
}

/// Variables are data values that belong to the environment
/// A variable's type is inferred upon the first usage
/// Therefore, once the parsing is done, any variable without a type is an error.
#[derive(Clone, Debug)]
pub struct Variable {
    id: String,
    data_type: Option<DataType>,
    content: Option<Constant>,
}

impl Variable {
    pub fn new(identifier: &str) -> Variable {
        Variable {
            id: identifier.to_string(),
            data_type: None,
            content: None,
        }
    }

    pub fn get_identifier(&self) -> &str{
        self.id.as_str()
    }

    pub fn get_type(&self) -> Option<DataType>{
        self.data_type.clone()
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(dtype) = &self.data_type {
            write!(f, "{:?} {}", dtype, self.id)
        } else {
            write!(f, "Unknown {}", self.id)
        }
    }
}