use std::fmt;
use std::rc::Rc;
use crate::yggl::structure::StructDecl;
use crate::yggl::function::Function;
use crate::yggl::environment::Variable;

pub enum YggType {
    Request,
    Reply,
    Message,
    Timer,
}

impl fmt::Display for YggType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            YggType::Request => "request",
            YggType::Reply => "reply",
            YggType::Message => "message",
            YggType::Timer => "timer"
        })
    }
}

pub enum Interface {
    Consumer(YggType, Rc<Variable>, u32),
    Producer(YggType, Rc<Variable>, u32, u32),
}

pub struct Handler {
    identifier: String,
    ytype: YggType,
    function: Rc<Function>,
}

impl Handler {
    pub fn new(identifier: String, ytype: YggType, function: Rc<Function>) -> Handler {
        Handler { identifier, ytype, function }
    }
}

pub struct Protocol {
    name: String,
    state: Rc<StructDecl>,
    init: Rc<Function>,
    interfaces: Vec<Interface>,
}

impl Protocol {
    pub fn new(name: String, state: Rc<StructDecl>, init: Rc<Function>, interfaces: Vec<Interface>)
               -> Protocol {
        Protocol { name, state, init, interfaces }
    }

    pub fn transpile(&self) -> String {
        let mut output = String::new();
        output.push_str(
            "NetworkConfig* ntconf = defineNetworkConfig(\"AdHoc\", 0, 5, 0, \"ledge\", YGG_filter);\n\
            ygg_runtime_init(ntconf);");


        output.push_str("ygg_runtime_start();");
        output
    }
}