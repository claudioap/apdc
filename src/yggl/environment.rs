use std::collections::{HashMap, LinkedList, HashSet};
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use crate::yggl::data::{Constant, DataType, Evaluable};
use crate::yggl::function::Function;
use crate::yggl::program::Include;
use crate::yggl::structure::StructDecl;

/// A program has a set of environments, which hold variable data.
pub struct Environment {
    // Environment symbols are static
    symbols: HashMap<String, Symbol>,
    scopes: LinkedList<Scope>,
    includes: HashSet<Include>,
}

#[allow(dead_code)]
impl Environment {
    pub fn new() -> Environment {
        let mut scopes = LinkedList::new();
        let scope = Scope::new();
        scopes.push_back(scope);
        Environment { scopes, symbols: HashMap::new(), includes: HashSet::new() }
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

    pub fn touch(&mut self, identifier: &str) -> Rc<Variable> {
        self.get_current_scope().touch(identifier)
    }

    pub fn declare(&mut self, identifier: &str, dtype: DataType) {
        self.get_current_scope().declare(identifier, dtype);
    }

    pub fn define(&mut self, identifier: &str, constant: Constant) -> Rc<Variable> {
        match self.get_definition_scope_mut(identifier) {
            Some(scope) => scope.define(identifier, constant),
            None => self.get_current_scope().define(identifier, constant)
        }
    }

    pub fn get_functions(&self) -> Vec<&Rc<Function>> {
        let mut set = vec!();
        for (_, symbol) in &self.symbols {
            if let Symbol::Function(function) = symbol {
                set.push(function);
            }
        }
        set
    }

    pub fn get_struct_defs(&self) -> Vec<&Rc<StructDecl>> {
        let mut set = vec!();
        for (_, symbol) in &self.symbols {
            if let Symbol::StructDecl(function) = symbol {
                set.push(function);
            }
        }
        set
    }

    pub fn get_includes(&self) -> &HashSet<Include> {
        &self.includes
    }

    pub fn get(&self, identifier: &str) -> Option<&Symbol> {
        match self.get_definition_scope(identifier) {
            Some(scope) => scope.get(identifier),
            None => match self.symbols.get(identifier) {
                Some(symbol) => Some(symbol),
                None => None
            }
        }
    }

    pub fn eval(&self, identifier: &str) -> Option<Symbol> {
        match self.get_definition_scope(identifier) {
            Some(scope) => scope.eval(identifier),
            None => None
        }
    }

    pub fn add_function(&mut self, function: Function) -> Rc<Function> {
        let name = function.get_name().to_string();
        let function_rc = Rc::new(function);
        self.symbols.insert(name, Symbol::Function(Rc::clone(&function_rc)));
        function_rc
    }

    pub fn add_struct_def(&mut self, struct_def: StructDecl) -> Rc<StructDecl> {
        let name = struct_def.get_name().to_string();
        let struct_def_rc = Rc::new(struct_def);
        self.symbols.insert(name, Symbol::StructDecl(Rc::clone(&struct_def_rc)));
        struct_def_rc
    }

    pub fn require_include(&mut self, include: Include) {
        self.includes.insert(include);
    }
}


/// A scope is a slice of the current environment.
/// Scopes can be stacked on top of each other, and variable resolution is done as a LIFO.
struct Scope {
    symbols: HashMap<String, Symbol>
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            symbols: HashMap::new(),
        }
    }

    fn touch(&mut self, identifier: &str) -> Rc<Variable> {
        if self.symbols.contains_key(identifier) {
            panic!("Declared same variable twice");
        } else {
            let var = Rc::new(Variable {
                id: identifier.to_string(),
                data_type: RefCell::new(None),
                content: RefCell::new(None),
            });
            self.symbols.insert(identifier.to_string(), Symbol::Variable(Rc::clone(&var)));
            var
        }
    }

    fn declare(&mut self, identifier: &str, dtype: DataType) -> Rc<Variable> {
        if let Some(symbol) = self.symbols.get(identifier) {
            if let Symbol::Variable(var) = symbol {
                if let Some(cdtype) = var.data_type() {
                    if dtype != cdtype {
                        panic!("Attempted to change a symbol data type");
                    }
                } else {
                    var.data_type.replace(Some(dtype));
                }
                Rc::clone(var)
            } else {
                unreachable!();
            }
        } else {
            let var = Rc::new(Variable {
                id: identifier.to_string(),
                data_type: RefCell::new(Some(dtype)),
                content: RefCell::new(None),
            });
            self.symbols.insert(identifier.to_string(), Symbol::Variable(Rc::clone(&var)));
            var
        }
    }

    fn define(&mut self, identifier: &str, constant: Constant) -> Rc<Variable> {
        if let Some(symbol) = self.symbols.get_mut(identifier) {
            if let Symbol::Variable(var) = symbol {
                match &var.get_type() {
                    Some(dt) if dt.clone() != constant.data_type() =>
                        panic!("Assigned to variable of different data type"),
                    Some(_) => { var.content.replace(Some(constant)); }
                    None => {
                        var.data_type.replace(Some(constant.data_type()));
                        var.content.replace(Some(constant));
                    }
                }
                Rc::clone(var)
            } else {
                unreachable!();
            }
        } else {
            let var = Rc::new(Variable {
                id: identifier.to_string(),
                data_type: RefCell::new(Some(constant.data_type())),
                content: RefCell::new(Some(constant)),
            });
            self.symbols.insert(identifier.to_string(), Symbol::Variable(Rc::clone(&var)));
            var
        }
    }

    pub fn is_defined(&self, identifier: &str) -> bool {
        self.symbols.contains_key(identifier)
    }

    pub fn get(&self, identifier: &str) -> Option<&Symbol> {
        self.symbols.get(identifier).clone()
    }

    pub fn eval(&self, identifier: &str) -> Option<Symbol> {
        match self.symbols.get(identifier) {
            Some(symbol) => {
                match symbol {
                    Symbol::Constant(_) | Symbol::Function(_) => Some(symbol.clone()),
                    Symbol::Variable(var) => {
                        if let Some(constant) = var.eval() {
                            Some(Symbol::Constant(constant))
                        } else {
                            panic!("Access to uninitialized variable");
                        }
                    }
                    Symbol::StructDecl(_) => { None }
                }
            }
            None => None
        }
    }

    #[allow(dead_code)]
    pub fn get_datatype(&self, identifier: &str) -> Option<DataType> {
        let symbol = self.symbols.get(identifier);
        if let Some(Symbol::Variable(var)) = symbol {
            var.data_type.borrow().clone()
        } else {
            None
        }
    }
}

/// Variables are data values that belong to the environment
/// A variable's type is inferred upon the first usage
/// Therefore, once the parsing is done, any variable without a type is an error.
#[derive(Clone)]
pub struct Variable {
    id: String,
    data_type: RefCell<Option<DataType>>,
    content: RefCell<Option<Constant>>,
}

#[allow(dead_code)]
impl Variable {
    pub fn get_identifier(&self) -> &str {
        self.id.as_str()
    }

    pub fn get_type(&self) -> Option<DataType> {
        self.data_type.borrow().clone()
    }

    pub fn get_content(&self) -> Option<Constant> {
        self.content.borrow().clone()
    }
}

impl Evaluable for Variable {
    fn data_type(&self) -> Option<DataType> {
        self.get_type()
    }

    fn eval(&self) -> Option<Constant> {
        self.content.borrow().clone()
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(dtype) = &self.data_type() {
            write!(f, "{} {}", dtype.transpile(), self.id)
        } else {
            write!(f, "Unknown {}", self.id)
        }
    }
}

#[derive(Clone)]
pub enum Symbol {
    Constant(Constant),
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    StructDecl(Rc<StructDecl>),
}