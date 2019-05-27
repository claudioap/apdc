use std::rc::Rc;
use std::fmt;
use std::hash::Hasher;
use std::collections::HashMap;
use crate::yggl::data::{DataType, Constant};
use crate::yggl::environment::Variable;
use crate::yggl::expression::Expression;
use std::cell::RefCell;

#[allow(dead_code)]
pub enum Attribute {
    Local(LocalAttribute),
    Foreign(ForeignAttribute),
}

pub struct LocalAttribute {
    name: String,
    dtype: RefCell<Option<DataType>>,
}

pub struct ForeignAttribute {
    name: String,
    dtype: RefCell<Option<DataType>>,
    _lambda: fn(Vec<Expression>) -> Option<Constant>,
}


impl Attribute {
    pub fn new(name: String, dtype: Option<DataType>) -> Attribute {
        Attribute::Local(LocalAttribute { name, dtype: RefCell::new(dtype) })
    }

    pub fn get_name(&self) -> &str {
        match self {
            Attribute::Local(local) => local.name.as_str(),
            Attribute::Foreign(foreign) => foreign.name.as_str(),
        }
    }

    pub fn data_type(&self) -> Option<DataType> {
        match self {
            Attribute::Local(local) => local.dtype.borrow().clone(),
            Attribute::Foreign(foreign) => foreign.dtype.borrow().clone()
        }
    }

    pub fn set_data_type(&self, dtype: Option<DataType>) {
        match self {
            Attribute::Local(local) => {
                let current = local.dtype.borrow().clone();
                if current != None {
                    if current != dtype {
                        panic!("Changed attribute datatype");
                    }
                    if dtype == None {
                        panic!("Nullified non-null attribute datatype.")
                    }
                }

                local.dtype.replace(dtype);
            }
            _ => { panic!("Cannot set the data type of a foreign attribute.") }
        }
    }

    pub fn access_transpile(&self, var: &Rc<Variable>) -> String {
        match self {
            Attribute::Local(attr) => format!("{}->{}", var.get_identifier(), attr.name),
            Attribute::Foreign(_attr) => unimplemented!(),
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
                if let Some(dtype) = &attribute.dtype.borrow().clone() {
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
    attributes: Vec<Rc<Attribute>>,
}


impl StructDecl {
    pub fn new(name: String, attributes: Vec<Rc<Attribute>>) -> StructDecl {
        StructDecl {
            name,
            attributes,
        }
    }

    pub fn get_name(&self) -> String {
        self.name.as_str().to_string()
    }

    pub fn get_attribute(&self, name: &str) -> Option<Rc<Attribute>> {
        for attribute in &self.attributes {
            if attribute.get_name() == name {
                return Some(Rc::clone(&attribute));
            }
        }
        None
    }

    pub fn transpile(&self) -> String {
        format!("{}\n", self)
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
            attribute_def.push_str(format!("    {},\n", attribute).as_str());
        }
        write!(f, "struct {} {{ \n{}}}", self.name, attribute_def)
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
