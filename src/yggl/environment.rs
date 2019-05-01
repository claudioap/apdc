use std::collections::{HashMap, LinkedList};
use crate::yggl::data::{Constant, DataType};
use std::fmt;
use crate::yggl::statement::Statement;

/// A program has a set of environments, which hold variable data.
#[derive(Clone)]
pub struct Environment<'a> {
    scopes: LinkedList<Scope<'a>>
}

#[allow(dead_code)]
impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        let mut scopes = LinkedList::new();
        scopes.push_back(Scope::new());
        Environment {
            scopes,
        }
    }

    pub fn declare(&mut self, _identifier: &str, _dtype: &DataType) -> Result<(), ()> {
        if let Some(_) = self.scopes.back() {
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn define(&mut self, _identifier: &str, _value: Constant) -> Result<(), ()> {
        if let Some(_) = self.scopes.back() {
            Ok(())
        } else {
            Err(())
        }
    }
}


/// A scope is a slice of the current environment.
/// Scopes can be stacked on top of each other, and variable resolution is done as a LIFO.
#[derive(Clone)]
struct Scope<'a> {
    constants: HashMap<String, Constant>,
    variables: HashMap<String, Variable>,
    statements: LinkedList<Statement<'a>>,
    parent: Option<Box<Scope<'a>>>,
}

#[allow(dead_code)]
impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {
            constants: HashMap::new(),
            variables: HashMap::new(),
            statements: LinkedList::new(),
            parent: None,
        }
    }

    fn define(&mut self, identifier: &str, dtype: &DataType) -> Result<(), ()> {
        if self.constants.contains_key(identifier) {
            return Err(());
        }
        if let Some(var) = self.variables.get_mut(identifier) {
            match &var.data_type {
                Some(_) => {}
                None => { var.data_type = Some(dtype.clone()) }
            }
        } else {
            let var = Variable {
                id: identifier.to_string(),
                data_type: Some(dtype.clone()),
            };
            self.variables.insert(identifier.to_string(), var);
        }
        return Ok(());
    }
}

/// Variables are data values that belong to the environment
/// A variable's type is inferred upon the first usage
/// Therefore, once the parsing is done, any variable without a error means an error.
#[derive(Clone, Debug)]
pub struct Variable {
    id: String,
    data_type: Option<DataType>,
}

impl Variable {
    pub fn new(identifier: &str) -> Variable{
        Variable {
            id: identifier.to_string(),
            data_type: None
        }
    }
    pub fn eval(&self, _env: &Environment) -> Result<Constant, ()> {
        return Err(());
    }
}
impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(dtype) = &self.data_type{
            write!(f, "{:?} {}", dtype, self.id)
        }else{
            write!(f, "Unknown {}", self.id)
        }
    }
}