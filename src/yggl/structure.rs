use std::borrow::Borrow;
use std::rc::Rc;
use std::fmt;
use std::hash::Hasher;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::parser::CompilationError;
use crate::yggl::data::DataType;
use crate::yggl::environment::Variable;
use crate::yggl::expression::Expression;
use crate::yggl::foreign::ForeignFunctionCall;
use crate::yggl::statement::Statement;

#[derive(Clone)]
pub enum Attribute {
    Local(Rc<LocalAttribute>),
    Foreign(Rc<ForeignAttribute>),
}

impl Attribute {
    pub fn name(&self) -> &str {
        match self {
            Attribute::Local(local) => local.name.as_str(),
            Attribute::Foreign(foreign) => foreign.name.as_str(),
        }
    }

    pub fn data_type(&self) -> Option<DataType> {
        match self {
            Attribute::Local(local) => local.data_type(),
            Attribute::Foreign(foreign) => foreign.data_type(),
        }
    }

    pub fn set_data_type(&self, dtype: Option<DataType>) -> Result<(), String> {
        match self {
            Attribute::Local(local) => {
                match &*(local.dtype.borrow()) {
                    Some(odtype) => {
                        if let Some(ndtype) = &dtype {
                            if ndtype == odtype {
                                Ok(())
                            } else {
                                Err(format!("Attempted to change a datatype"))
                            }
                        } else {
                            Err(format!("Attempted to nullify a datatype"))
                        }
                    }
                    None => {
                        local.dtype.replace(dtype);
                        Ok(())
                    }
                }
            }
            Attribute::Foreign(foreign) => {
                if foreign.data_type() == dtype {
                    Ok(())
                } else {
                    Err(format!("Attempted to change a foreign attribute data type"))
                }
            }
        }
    }
}

pub struct LocalAttribute {
    name: String,
    dtype: RefCell<Option<DataType>>,
}

impl LocalAttribute {
    pub fn new(name: String, dtype: Option<DataType>) -> LocalAttribute {
        LocalAttribute { name, dtype: RefCell::new(dtype) }
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn read_statement(&self, variable: Rc<Variable>, this: Rc<LocalAttribute>, arguments: Vec<Expression>)
                          -> Result<Expression, CompilationError> {
        if arguments.len() > 0 {
            return Err(CompilationError::new(
                0, 0, "".to_string(),
                "Local attributes do not take arguments".to_string()));
        }
        Ok(Expression::AttributeAccess(variable, this))
    }

    pub fn write_statement(&self, variable: Rc<Variable>, this: Rc<LocalAttribute>, mut arguments: Vec<Expression>)
                           -> Result<Statement, CompilationError> {
        if arguments.len() > 0 {
            return Err(CompilationError::new(
                0, 0, "".to_string(),
                "Local attribute write takes one argument".to_string()));
        }
        let value = arguments.remove(0);
        Ok(Statement::AttributeAssignment(variable, this, value))
    }

    pub fn data_type(&self) -> Option<DataType> {
        self.dtype.borrow().clone()
    }

    pub fn set_data_type(&self, dtype: DataType) {
        let current = self.dtype.borrow().clone();
        let new = Some(dtype);
        if current != None {
            if current != new {
                panic!("Changed attribute datatype");
            }
        }
        self.dtype.replace(new);
    }
}

pub struct ForeignAttribute {
    name: String,
    dtype: RefCell<Option<DataType>>,
    handler: fn(Rc<Variable>, Vec<Expression>) -> Box<dyn ForeignFunctionCall>
}

impl ForeignAttribute {
    pub fn new(name: String, dtype: Option<DataType>,
               handler: fn(Rc<Variable>, Vec<Expression>) -> Box<dyn ForeignFunctionCall>)
               -> ForeignAttribute {
        ForeignAttribute {
            name,
            dtype: RefCell::new(dtype),
            handler
        }
    }


    pub fn handle(&self, variable: Rc<Variable>, arguments: Vec<Expression>)
                     -> Result<Box<dyn ForeignFunctionCall>, CompilationError> {
        Ok((self.handler)(variable, arguments))
    }

    pub fn data_type(&self) -> Option<DataType> {
        self.dtype.borrow().clone()
    }
}

impl std::cmp::Eq for Attribute {}

impl std::cmp::PartialEq for Attribute {
    fn eq(&self, other: &Attribute) -> bool {
        self.name() == other.name()
    }
}


impl std::hash::Hash for Attribute {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name().hash(state);
    }
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(dtype) = self.data_type() {
            write!(f, "{} {}", dtype.transpile(), self.name())
        } else {
            write!(f, "unknown {}", self.name())
        }
    }
}

pub struct StructDecl {
    name: String,
    attributes: Vec<Attribute>,
    exported: bool,
    implicit: bool,
}

impl StructDecl {
    pub fn new(name: String, attributes: Vec<Attribute>) -> StructDecl {
        StructDecl {
            name,
            attributes,
            exported: false,
            implicit: false,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.as_str().to_string()
    }

    pub fn get_attribute(&self, name: &str) -> Option<Attribute> {
        for attribute in &self.attributes {
            if attribute.name() == name {
                return Some(attribute.clone());
            }
        }
        None
    }

    pub fn is_exported(&self) -> bool {
        self.exported
    }

    pub fn set_exported(&mut self) {
        self.exported = true;
    }

    pub fn set_implicit(&mut self) {
        self.implicit = true;
    }

    pub fn transpile(&self) -> String {
        if self.implicit {
            "".to_string()
        } else {
            format!("typedef {}\n", self)
        }
    }
}

impl std::cmp::PartialEq for StructDecl {
    fn eq(&self, _other: &StructDecl) -> bool {
        unimplemented!()
    }
}

impl std::cmp::Eq for StructDecl {}

impl std::hash::Hash for StructDecl {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        unimplemented!()
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attribute_def = String::new();
        for attribute in &self.attributes {
            if let Attribute::Local(_) = attribute.borrow() {
                attribute_def.push_str(format!("    {},\n", attribute).as_str());
            }
        }
        write!(f, "struct {} {{ \n{}}} {};", self.name, attribute_def, self.name)
    }
}

impl fmt::Debug for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub struct StructDef {
    definition: Rc<StructDecl>,
    _fields: HashMap<Rc<Attribute>, Option<Rc<Variable>>>,
}

impl StructDef {
    pub fn new(definition: Rc<StructDecl>, _fields: HashMap<Rc<Attribute>, Option<Rc<Variable>>>) -> StructDef {
        StructDef {
            definition,
            _fields,
        }
    }

    pub fn get_declaration(&self) -> Rc<StructDecl> {
        Rc::clone(&self.definition)
    }
}
