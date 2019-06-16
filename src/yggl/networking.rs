use std::rc::Rc;
use crate::yggl::environment::Variable;

pub enum Address {
    Octet(Vec<u8>),
    Variable(Rc<Variable>),
    Broadcast,
}