use std::rc::Rc;
use crate::yggl::data::DataType;
use crate::yggl::structure::StructDecl;
use crate::yggl::protocol::ProtocolDef;
use crate::yggl::expression::Expression;
use crate::yggl::function::Function;
use crate::yggl::environment::Variable;
use crate::yggl::networking::Address;

// ############### Foreign types ###############
#[derive(Clone, PartialEq, Hash, Debug)]
pub enum ForeignType {
    //proto_def
    ProtoDef,
    // uuid_t
    UUID,
    // list
    List,
    Timer,
    Message,
    Request,
    Response,
    Notification,
}

pub trait ForeignObject {
    fn datatype(&self) -> DataType;
}

pub struct ProtoDef {}

impl ForeignObject for ProtoDef {
    fn datatype(&self) -> DataType {
        DataType::Reference(Box::new(DataType::Foreign(ForeignType::ProtoDef)))
    }
}

pub struct Message {
    variable: Rc<Variable>,
}

impl Message {
    pub fn new(variable: Rc<Variable>) -> Message {
        let message = Message { variable: Rc::clone(&variable) };
        variable.set_type(message.datatype());
        message
    }

    pub fn get_init_call(&self, address: Address) -> MessageInitCall {
        MessageInitCall::new(Rc::clone(&self.variable), address)
    }

    pub fn get_dispatch_call(&self) -> MessageDispatchCall {
        MessageDispatchCall::new(Rc::clone(&self.variable))
    }
}

impl ForeignObject for Message {
    fn datatype(&self) -> DataType {
        DataType::Reference(Box::new(DataType::Foreign(ForeignType::Message)))
    }
}

// ############### Foreign functions ###############
pub trait ForeignFunctionCall {
    fn get_name(&self) -> &str;
    fn get_parameter_types(&self) -> Vec<DataType>;
    fn get_args(&self) -> Vec<Expression>;
    fn return_type(&self) -> Option<DataType>;
    fn transpile(&self) -> String {
        let mut output = format!("{}(", self.get_name());
        let args = self.get_args();
        for argument in &args {
            output.push_str(format!("{},", argument.transpile()).as_str())
        }
        if !args.is_empty() {
            output.pop();
        }
        output.push_str(");");
        output
    }
}

// --------------- Protocol ---------------
pub struct ProtoCreateCall {
    state: Rc<StructDecl>,
    protocol: Rc<ProtocolDef>,
}

impl ForeignFunctionCall for ProtoCreateCall {
    fn get_name(&self) -> &str {
        "create_protocol_definition"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Int, DataType::String, DataType::Struct(Rc::clone(&self.state))]
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        Some(DataType::Foreign(ForeignType::ProtoDef))
    }
}


pub struct ProtoAddMLoopCall {}

impl ForeignFunctionCall for ProtoAddMLoopCall {
    fn get_name(&self) -> &str {
        "proto_def_add_protocol_main_loop"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}


pub struct ProtoAddProducedCall {}

impl ForeignFunctionCall for ProtoAddProducedCall {
    fn get_name(&self) -> &str {
        "proto_def_add_produced_events"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}


pub struct ProtoAddConsumedCall {}

impl ForeignFunctionCall for ProtoAddConsumedCall {
    fn get_name(&self) -> &str {
        "proto_def_add_consumed_event"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

// --------------- Handlers ---------------
pub struct ProtoDefAddMsgHCall {
    protocol: Rc<ProtocolDef>,
    handler: Rc<Function>,
}

impl ProtoDefAddMsgHCall {
    pub fn new(protocol: Rc<ProtocolDef>, handler: Rc<Function>) -> ProtoDefAddMsgHCall {
        ProtoDefAddMsgHCall { protocol, handler }
    }
}

impl ForeignFunctionCall for ProtoDefAddMsgHCall {
    fn get_name(&self) -> &str {
        "proto_def_add_msg_handler"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Function]
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct ProtoDefAddTimerHCall {
    protocol: Rc<ProtocolDef>,
    handler: Rc<Function>,
}

impl ProtoDefAddTimerHCall {
    pub fn new(protocol: Rc<ProtocolDef>, handler: Rc<Function>) -> ProtoDefAddTimerHCall {
        ProtoDefAddTimerHCall { protocol, handler }
    }
}

impl ForeignFunctionCall for ProtoDefAddTimerHCall {
    fn get_name(&self) -> &str {
        "proto_def_add_timer_handler"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct ProtoDefAddEventHCall {
    protocol: Rc<ProtocolDef>,
    handler: Rc<Function>,
}

impl ProtoDefAddEventHCall {
    pub fn new(protocol: Rc<ProtocolDef>, handler: Rc<Function>) -> ProtoDefAddEventHCall {
        ProtoDefAddEventHCall { protocol, handler }
    }
}

impl ForeignFunctionCall for ProtoDefAddEventHCall {
    fn get_name(&self) -> &str {
        "proto_def_add_event_handler"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct ProtoDefAddRequestHCall {
    protocol: Rc<ProtocolDef>,
    handler: Rc<Function>,
}

impl ProtoDefAddRequestHCall {
    pub fn new(protocol: Rc<ProtocolDef>, handler: Rc<Function>) -> ProtoDefAddRequestHCall {
        ProtoDefAddRequestHCall { protocol, handler }
    }
}

impl ForeignFunctionCall for ProtoDefAddRequestHCall {
    fn get_name(&self) -> &str {
        "proto_def_add_request_handler"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        unimplemented!()
    }

    fn get_args(&self) -> Vec<Expression> {
        unimplemented!()
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

// --------------- Data methods ---------------
pub struct MessageInitCall {
    variable: Rc<Variable>,
    address: Address,
}

impl MessageInitCall {
    pub fn new(variable: Rc<Variable>, address: Address) -> MessageInitCall {
        MessageInitCall { variable, address }
    }
}

impl ForeignFunctionCall for MessageInitCall {
    fn get_name(&self) -> &str {
        match self.address {
            Address::Broadcast => "YggMessage_initBcast",
            _ => unimplemented!()
        }
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Message)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::Variable(Rc::clone(&self.variable))]
    }

    fn return_type(&self) -> Option<DataType> {
        Some(DataType::Int)
    }
}

pub struct MessageDispatchCall {
    variable: Rc<Variable>,
}

impl MessageDispatchCall {
    pub fn new(variable: Rc<Variable>) -> MessageDispatchCall {
        MessageDispatchCall { variable }
    }
}

impl ForeignFunctionCall for MessageDispatchCall {
    fn get_name(&self) -> &str {
        "dispatch"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Message)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::Variable(Rc::clone(&self.variable))]
    }

    fn return_type(&self) -> Option<DataType> {
        Some(DataType::Int)
    }
}