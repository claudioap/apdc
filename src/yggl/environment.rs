use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use crate::yggl::data::{Constant, DataType};
use crate::yggl::function::Function;
use crate::yggl::protocol::{Include, YggType};
use crate::yggl::structure::StructDecl;
use crate::yggl::embedded::list;

// Count of auxiliary variables
static mut AUX_COUNT: i32 = 0;
// Built in types load flag
static mut BUILT_IN_LOADED: bool = false;
// Count of issued labels
static mut LABEL_COUNT: u8 = 0;

/// A program has a set of environments, which hold variable data.
/// A scope is a slice of the current environment.
/// Scopes can be stacked on top of each other, and variable resolution is done as a LIFO.
pub struct Environment {
    // Environment symbols are static
    scopes: VecDeque<HashMap<String, Symbol>>,
    includes: HashSet<Include>,
}

#[allow(dead_code)]
impl Environment {
    pub fn new() -> Environment {
        let mut scopes = VecDeque::new();
        let mut scope = HashMap::new();
        unsafe {
            if !BUILT_IN_LOADED {
                BUILT_IN_LOADED = true;
                scope.insert("list".to_string(), Symbol::StructDecl(Rc::new(list())));
            }
        }
        scopes.push_back(scope);
        Environment { scopes, includes: HashSet::new() }
    }

    /// Adds a scope to the environment stack
    pub fn push_scope(&mut self) {
        let scope = HashMap::new();
        self.scopes.push_back(scope);
    }

    /// Removes a scope from the environment stack
    pub fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Attempted to remove last scope from the environment.")
        }
        self.scopes.pop_back();
    }

    /// Obtains the current scope
    fn get_current_scope(&mut self) -> &mut HashMap<String, Symbol> {
        self.scopes.back_mut().unwrap()
    }

    /// Finds the first scope where an identifier is defined (LIFO)
    fn get_definition_scope(&self, identifier: &str) -> Option<&HashMap<String, Symbol>> {
        for scope in &self.scopes {
            if scope.contains_key(identifier) {
                return Some(scope);
            }
        }
        None
    }

    /// Declares a variable of a an unknown type in the current scope
    /// (it is up to the receiver to set the type, but possibly it shouldn't )
    pub fn declare(&mut self, identifier: &str) -> Rc<Variable> {
        let scope = self.get_current_scope();
        if scope.contains_key(identifier) {
            panic!("Declared same variable ({}) twice in the same scope.", identifier);
        } else {
            let var = Rc::new(Variable {
                id: identifier.to_string(),
                declared: RefCell::new(false),
                data_type: RefCell::new(None),
                content: RefCell::new(None),
            });
            scope.insert(identifier.to_string(), Symbol::Variable(Rc::clone(&var)));
            var
        }
    }

    pub fn declare_aux(&mut self) -> Rc<Variable> {
        unsafe {
            let identifier = format!("aux_var_{}", AUX_COUNT);
            AUX_COUNT += 1;
            self.declare(identifier.as_str())
        }
    }

    pub fn set_defines(&mut self, identifier: &str, constant: Constant) {
        let scope = self.scopes.front_mut().unwrap();
        // TODO check if already defined IN ANY SCOPE
        // TODO assert that vars cannot shadow this identifier.
        scope.insert(
            identifier.to_string(),
            Symbol::Define(Define::new(identifier.to_string(), constant)));
    }


    pub fn get_defines(&self) -> Vec<Define> {
        let mut set = vec!();
        for symbol in self.scopes.front().unwrap().values() {
            if let Symbol::Define(ref define) = symbol {
                set.push(define.clone());
            }
        }
        set
    }

    /// Obtains every function that is present in this environment
    pub fn get_functions(&self) -> Vec<Rc<Function>> {
        let mut set = vec!();
        for symbol in self.scopes.front().unwrap().values() {
            if let Symbol::Function(ref function) = symbol {
                set.push(Rc::clone(function));
            }
        }
        set
    }

    /// Obtains every structure definition that is present in this environment
    pub fn get_struct_decls(&self) -> Vec<Rc<StructDecl>> {
        let mut set = vec!();
        for symbol in self.scopes.front().unwrap().values() {
            if let Symbol::StructDecl(ref decl) = symbol {
                set.push(Rc::clone(decl));
            }
        }
        set
    }

    /// Gets every static symbol into a vector
    pub fn get_static_symbols(&self) -> Vec<Symbol> {
        let mut symbols = vec!();
        for symbol in self.scopes.front().unwrap().values() {
            symbols.push(symbol.clone());
        }
        symbols
    }

    /// (Deprecated) Gets every include that was required in this environment
    pub fn get_includes(&self) -> &HashSet<Include> {
        &self.includes
    }

    /// Gets a symbol by its identifier
    pub fn get(&self, identifier: &str) -> Option<Symbol> {
        match self.get_definition_scope(identifier) {
            Some(scope) => match scope.get(identifier) {
                Some(symbol) => { Some(symbol.clone()) }
                None => None
            },
            None => None
        }
    }

    /// Adds a function symbol to the static scope of this environment
    /// TODO forbid the symbol from being in use in the remaining scopes
    pub fn add_function(&mut self, function: Function) -> Rc<Function> {
        let name = function.get_name().to_string();
        let function_rc = Rc::new(function);
        let static_scope = self.scopes.front_mut().unwrap();
        static_scope.insert(name, Symbol::Function(Rc::clone(&function_rc)));
        function_rc
    }

    /// Adds a structure declaration symbol to the static scope of this environment
    /// TODO forbid the symbol from being in use in the remaining scopes
    pub fn add_struct_decl(&mut self, struct_decl: StructDecl) -> Rc<StructDecl> {
        let name = struct_decl.get_name().to_string();
        let struct_def_rc = Rc::new(struct_decl);
        let static_scope = self.scopes.front_mut().unwrap();
        static_scope.insert(name, Symbol::StructDecl(Rc::clone(&struct_def_rc)));
        struct_def_rc
    }

    pub fn add_label(&mut self, label: Label) -> Rc<Label> {
        let name = label.get_identifier().to_string();
        let static_scope = self.scopes.front_mut().unwrap();
        let label_rc = Rc::new(label);
        static_scope.insert(name, Symbol::Label(Rc::clone(&label_rc)));
        label_rc
    }

    /// Imports a vector of static symbols
    /// Meant to be used to import static symbols from the program environment
    /// into a function's environment
    pub fn push_static(&mut self, global_static: Vec<Symbol>) {
        let static_scope = self.scopes.front_mut().unwrap();
        for symbol in global_static {
            match symbol {
                Symbol::StructDecl(decl) => {
                    static_scope.insert(
                        decl.get_name(),
                        Symbol::StructDecl(decl));
                }
                Symbol::Variable(var) => {
                    static_scope.insert(
                        var.get_identifier().to_string(),
                        Symbol::Variable(var));
                }
                Symbol::Function(function) => {
                    static_scope.insert(
                        function.get_name().to_string(),
                        Symbol::Function(function));
                }
                Symbol::Define(define) => {
                    static_scope.insert(
                        define.get_identifier().to_string(),
                        Symbol::Define(define));
                }
                Symbol::Label(label) => {
                    static_scope.insert(
                        label.get_identifier().to_string(),
                        Symbol::Label(label));
                }
            }
        }
    }

    /// (Deprecated) Add an include that was required in this environment's
    /// TODO Do this after parsing, probably after the annotation tasks
    pub fn require_include(&mut self, include: Include) {
        self.includes.insert(include);
    }

    /// Debug helper that prints a snapshot of the current environment
    pub fn print_snapshot(&self) {
        for scope in &self.scopes {
            println!("----------------------------");
            for symbol in scope.values() {
                match symbol {
                    Symbol::Variable(ref var) => {
                        if let Some(dtype) = var.data_type() {
                            println!("{} {}", dtype.transpile(), var.get_identifier());
                        } else {
                            println!("Unknown {}", var.get_identifier());
                        }
                    }
                    Symbol::StructDecl(ref struct_decl) => {
                        println!("struct {}", struct_decl.get_name());
                    }
                    Symbol::Function(ref fun) => {
                        println!("fun {}", fun.get_name());
                    }
                    _ => {}
                }
            }
            println!("----------------------------");
        }
    }

    /// Obtains the data type of the symbol that matches with that identifier
    /// Matching is done with the scopes being considered as a LIFO
    pub fn get_datatype(&self, identifier: &str) -> Option<DataType> {
        if let Some(Symbol::Variable(var)) = self.get(identifier) {
            var.data_type.borrow().clone()
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub struct Define {
    id: String,
    value: Constant,
}

impl Define {
    pub fn new(id: String, value: Constant) -> Define {
        Define { id, value }
    }

    pub fn get_identifier(&self) -> &str {
        self.id.as_str()
    }

    pub fn value(&self) -> Constant {
        self.value.clone()
    }
}


/// Variables are data values that belong to the environment
/// A variable's type is inferred upon the first usage
/// Therefore, once the parsing is done, any variable without a type is an error.
#[derive(Clone)]
pub struct Variable {
    id: String,
    declared: RefCell<bool>,
    data_type: RefCell<Option<DataType>>,
    content: RefCell<Option<Constant>>,
}

#[allow(dead_code)]
impl Variable {
    pub fn get_identifier(&self) -> &str {
        self.id.as_str()
    }

    pub fn has_type(&self) -> bool {
        self.data_type.borrow().is_some()
    }

    pub fn data_type(&self) -> Option<DataType> {
        self.data_type.borrow().clone()
    }

    pub fn set_type(&self, dtype: DataType) {
        self.data_type.replace(Some(dtype));
    }

    pub fn content(&self) -> Option<Constant> {
        self.content.borrow().clone()
    }

    pub fn set_content(&self, c: Constant) {
        // TODO: Validate
        self.content.replace(Some(c));
    }

    pub fn is_declared(&self) -> bool {
        self.declared.borrow().clone()
    }

    pub fn set_declared(&self) {
        self.declared.replace(true);
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

pub struct Label {
    identifier: String,
    code: u8,
    ytype: YggType,
}

impl Label {
    pub fn new(identifier: String, ytype: YggType) -> Label {
        unsafe {
            let code = LABEL_COUNT;
            LABEL_COUNT += 1;
            Label { identifier, code, ytype }
        }
    }

    pub fn get_identifier(&self) -> &str {
        self.identifier.as_str()
    }

    pub fn get_type(&self) -> YggType {
        self.ytype.clone()
    }
}

#[derive(Clone)]
pub enum Symbol {
    Define(Define),
    Variable(Rc<Variable>),
    Function(Rc<Function>),
    StructDecl(Rc<StructDecl>),
    Label(Rc<Label>),
}