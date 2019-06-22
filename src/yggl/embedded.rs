use std::rc::Rc;
use crate::yggl::structure::{StructDecl, ForeignAttribute, Attribute};
use crate::yggl::foreign::ListAddHeadCall;

pub fn list() -> StructDecl {
    let add_op = Attribute::Foreign(Rc::new(ForeignAttribute::new(
        "add".to_string(),
        None,
        |this, mut expr| {
            if expr.len() != 1 { panic!() }
            Box::new(ListAddHeadCall::new(this, expr.remove(0)))
        }
    )));
    let mut decl = StructDecl::new("list".to_string(), vec![add_op]);
    decl.set_implicit();
    decl
}