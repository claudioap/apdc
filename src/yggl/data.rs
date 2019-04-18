use std::{fmt, ops};

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum DataType { Bool, Int, Float, Char, String, Function }

/// Constants are either hardcoded literals or evaluated values
#[derive(Clone)]
#[allow(dead_code)]
pub enum Constant {
    Bool(bool),
    Int(i32),
    Float(f32),
    String(String),
    Char(char),
}

impl Constant{
    pub fn data_type(&self) -> DataType{
        match self{
            Constant::Int(_) => DataType::Int,
            Constant::Float(_) => DataType::Float,
            Constant::String(_) => DataType::String,
            Constant::Char(_) => DataType::Char,
            Constant::Bool(_) => DataType::Bool
        }
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
//            Constant::Null => write!(f, "null")
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