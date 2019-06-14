use std::rc::Rc;
use crate::yggl::structure::StructDecl;
use crate::yggl::function::Function;

pub struct Protocol {
    name: String,
    state: Rc<StructDecl>,
    init: Rc<Function>,
}

impl Protocol {
    pub fn new(name: String, state: Rc<StructDecl>, init: Rc<Function>) -> Protocol {
        return Protocol { name, state, init };
    }

    pub fn transpile(&self) -> String {
        "TODO".to_string()
    }
}