use std::{fmt, ops};
use std::rc::Rc;
use crate::yggl::structure::{StructDef, StructDecl};
use std::cmp::Ordering;

#[allow(dead_code)]
#[derive(Clone, PartialEq, Hash, Debug)]
pub enum DataType {
    Bool,
    Int,
    Float,
    Char,
    String,
    Function,
    Struct(Rc<StructDecl>),
    Reference(Box<DataType>),
}


#[allow(dead_code)]
impl DataType {
    pub fn transpile(&self) -> String {
        match self {
            DataType::Bool => "int".to_string(),
            DataType::Int => "int".to_string(),
            DataType::Float => "float".to_string(),
            DataType::Char => "char".to_string(),
            DataType::String | DataType::Function =>
                panic!("Not meant to be transpiled."),
            DataType::Struct(ref decl) => format!("struct {}", decl.get_name()),
            DataType::Reference(ref dtype) => format!("{} *", dtype.transpile()),
        }
    }

    pub fn has_attribs(&self) -> bool {
        match self {
            DataType::Struct(_) => true,
            _ => false
        }
    }
}

pub trait Evaluable {
    fn data_type(&self) -> Option<DataType>;
    fn eval(&self) -> Option<Constant>;
}

/// Constants are either hardcoded literals or evaluated values
#[derive(Clone)]
#[allow(dead_code)]
pub enum Constant {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Char(char),
    Structure(Rc<StructDef>),
    Reference(Box<Constant>),
}

impl Constant {
    pub fn pow(&self, c: &Constant) -> Constant {
        match self {
            Constant::Int(i1) => {
                if let Constant::Int(i2) = c {
                    Constant::Int(i1.pow(*i2 as u32))
                } else {
                    unimplemented!("Power of different data types")
                }
            }
            Constant::Float(f1) => {
                if let Constant::Float(f2) = c {
                    Constant::Float(f1.powf(*f2))
                } else {
                    unimplemented!("Power of different data types")
                }
            }
            _ => unimplemented!("Power of unsupported data types")
        }
    }

    pub fn parse_number(number: &str) -> Constant {
        if number.contains('.') || number.contains('e') {
            Constant::Float(number.parse().unwrap())
        } else {
            Constant::Int(number.parse().unwrap())
        }
    }

    pub fn truth_value(&self) -> bool {
        match self {
            &Constant::Bool(b) => b,
            &Constant::Int(i) => i != 0,
            &Constant::Float(f) => f != 0.0,
            &Constant::String(_) => true,
            &Constant::Char(c) => c != '\0',
            &Constant::Structure(_) => panic!("Structures have no truth values"),
            &Constant::Reference(ref c) => c.truth_value()
        }
    }

    pub fn data_type(&self) -> DataType {
        match self {
            Constant::Int(_) => DataType::Int,
            Constant::Float(_) => DataType::Float,
            Constant::String(_) => DataType::String,
            Constant::Char(_) => DataType::Char,
            Constant::Bool(_) => DataType::Bool,
            Constant::Structure(s) => DataType::Struct(s.get_declaration()),
            Constant::Reference(c) => DataType::Reference(Box::new(c.data_type()))
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(i) => write!(f, "{}", i),
            Constant::String(s) => write!(f, "\"{}\"", s),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::Structure(_) => panic!("structures are not printable"),
            Constant::Reference(c) => write!(f, "{}", c),
        }
    }
}

impl std::cmp::PartialEq for Constant {
    fn eq(&self, _other: &Constant) -> bool {
        unimplemented!()
    }
}

impl std::cmp::PartialOrd for Constant {
    fn partial_cmp(&self, _other: &Constant) -> Option<Ordering> {
        unimplemented!()
    }
}

// Arithmetic operations within constants
impl ops::Add<Constant> for Constant {
    type Output = Constant;

    fn add(self, _rhs: Constant) -> Constant {
        match self {
            Constant::Int(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Int(i + j),
                    Constant::Float(j) => Constant::Float(i as f32 + j),
                    _ => panic!()
                }
            }
            Constant::Float(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Float(i + j as f32),
                    Constant::Float(j) => Constant::Float(i as f32 + j),
                    _ => panic!()
                }
            }
            _ => panic!("Added int to null"),
        }
    }
}

impl ops::Sub<Constant> for Constant {
    type Output = Constant;

    fn sub(self, _rhs: Constant) -> Constant {
        match self {
            Constant::Int(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Int(i - j),
                    Constant::Float(j) => Constant::Float(i as f32 - j),
                    _ => panic!()
                }
            }
            Constant::Float(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Float(i - j as f32),
                    Constant::Float(j) => Constant::Float(i as f32 - j),
                    _ => panic!()
                }
            }
            _ => panic!("Added int to null"),
        }
    }
}

impl ops::Mul<Constant> for Constant {
    type Output = Constant;

    fn mul(self, _rhs: Constant) -> Constant {
        match self {
            Constant::Int(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Int(i * j),
                    Constant::Float(j) => Constant::Float(i as f32 * j),
                    _ => panic!()
                }
            }
            Constant::Float(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Float(i * j as f32),
                    Constant::Float(j) => Constant::Float(i as f32 * j),
                    _ => panic!()
                }
            }
            _ => panic!("Added int to null"),
        }
    }
}

impl ops::Div<Constant> for Constant {
    type Output = Constant;

    fn div(self, _rhs: Constant) -> Constant {
        match self {
            Constant::Int(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Int(i / j),
                    Constant::Float(j) => Constant::Float(i as f32 / j),
                    _ => panic!()
                }
            }
            Constant::Float(i) => {
                match _rhs {
                    Constant::Int(j) => Constant::Float(i / j as f32),
                    Constant::Float(j) => Constant::Float(i as f32 / j),
                    _ => panic!()
                }
            }
            _ => panic!("Added int to null"),
        }
    }
}