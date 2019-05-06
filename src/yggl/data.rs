use std::{fmt, ops};

#[allow(dead_code)]
#[derive(Clone, PartialEq, Debug)]
pub enum DataType { Bool, Int, Float, Char, String, Function }

impl DataType {
    pub fn transpile(&self) -> &str {
        match self {
            DataType::Bool => "int",
            DataType::Int => "int",
            DataType::Float => "float",
            DataType::Char => "char",
            DataType::String => panic!("Not meant to be transpiled."),
            DataType::Function => panic!("Not meant to be transpiled."),
        }
    }
}

pub trait Evaluable {
    fn data_type(&self) -> Option<DataType>;
}

/// Constants are either hardcoded literals or evaluated values
#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Constant {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Char(char),
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
}

impl Evaluable for Constant {
    fn data_type(&self) -> Option<DataType> {
        Some(match self {
            Constant::Int(_) => DataType::Int,
            Constant::Float(_) => DataType::Float,
            Constant::String(_) => DataType::String,
            Constant::Char(_) => DataType::Char,
            Constant::Bool(_) => DataType::Bool
        })
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Bool(b) => write!(f, "{}", b),
            Constant::Int(i) => write!(f, "{}", i),
            Constant::Float(i) => write!(f, "{}", i),
            Constant::String(s) => write!(f, "{}", s),
            Constant::Char(c) => write!(f, "{}", c),
        }
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