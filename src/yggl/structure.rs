use std::rc::Rc;
use std::fmt;
use std::hash::Hasher;
use std::collections::HashMap;
use crate::yggl::data::{DataType, Constant};
use crate::yggl::environment::Symbol;
use crate::yggl::expression::Expression;

#[allow(dead_code)]
pub enum Attribute {
    Local(LocalAttribute),
    Foreign(ForeignAttribute),
}

pub struct LocalAttribute {
    name: String,
    dtype: Option<DataType>,
}

pub struct ForeignAttribute {
    name: String,
    _dtype: Option<DataType>,
    _lambda: fn(Vec<Expression>) -> Option<Constant>,
}


impl Attribute {
    pub fn new(name: String, dtype: Option<DataType>) -> Attribute {
        Attribute::Local(LocalAttribute { name, dtype })
    }

    pub fn get_name(&self) -> &str {
        match self {
            Attribute::Local(local) => local.name.as_str(),
            Attribute::Foreign(foreign) => foreign.name.as_str(),
        }
    }
}

impl std::cmp::Eq for Attribute {}

impl std::cmp::PartialEq for Attribute {
    fn eq(&self, other: &Attribute) -> bool {
        self.get_name() == other.get_name()
    }
}


impl std::hash::Hash for Attribute {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_name().hash(state);
    }
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Attribute::Local(attribute) => {
                if let Some(dtype) = &attribute.dtype {
                    write!(f, "{} {}", dtype.transpile(), attribute.name)
                } else {
                    write!(f, "unknown {}", attribute.name)
                }
            }
            _ => { write!(f, "TODO") }
        }
    }
}

pub struct StructDecl {
    name: String,
    attributes: Vec<Attribute>,
}


impl StructDecl {
    pub fn new(name: String, attributes: Vec<Attribute>) -> StructDecl {
        StructDecl {
            name,
            attributes,
        }
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn transpile(&self) -> String {
        format!("{}\n", self)
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attribute_def = String::new();
        for attribute in &self.attributes {
            attribute_def.push_str(format!("    {},\n", attribute).as_str());
        }
        write!(f, "struct {} {{ \n{}}}", self.name, attribute_def)
    }
}

pub struct StructDef {
    definition: Rc<StructDecl>,
    _fields: HashMap<Rc<Attribute>, Option<Symbol>>,
}

impl StructDef {
    pub fn new(definition: Rc<StructDecl>, _fields: HashMap<Rc<Attribute>, Option<Symbol>>) -> StructDef {
        StructDef {
            definition,
            _fields,
        }
    }

    pub fn get_declaration(&self) -> Rc<StructDecl> {
        Rc::clone(&self.definition)
    }
}
