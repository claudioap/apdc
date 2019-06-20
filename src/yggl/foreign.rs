use std::rc::Rc;
use std::fmt;
use crate::yggl::data::{DataType, Constant};
use crate::yggl::structure::StructDecl;
use crate::yggl::protocol::ProtocolDef;
use crate::yggl::expression::{Expression, UnaryOperation};
use crate::yggl::function::Function;
use crate::yggl::environment::Variable;
use crate::yggl::networking::Address;
use crate::yggl::timer::TimerType;

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
    Event,
}

impl fmt::Display for ForeignType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ForeignType::ProtoDef => write!(f, "proto_def"),
            ForeignType::UUID => write!(f, "uuid_T"),
            ForeignType::List => write!(f, "List"),
            ForeignType::Timer => write!(f, "YggTimer"),
            ForeignType::Message => write!(f, "YggMessage"),
            ForeignType::Request => write!(f, "YggRequest"),
            ForeignType::Response => write!(f, "YggResponse"),
            ForeignType::Event => write!(f, "YggEvent")
        }
    }
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
        DataType::Foreign(ForeignType::Message)
    }
}

pub struct Timer {
    variable: Rc<Variable>,
    ttype: TimerType,
    wait: u64,
    period: u64,
}

impl Timer {
    pub fn new(variable: Rc<Variable>, ttype: TimerType, wait: u64, period: u64) -> Timer {
        let timer = Timer { variable: Rc::clone(&variable), ttype, wait, period };
        variable.set_type(timer.datatype());
        timer
    }

    pub fn get_init_call(&self) -> TimerInitCall {
        TimerInitCall::new(Rc::clone(&self.variable))
    }

    pub fn get_set_call(&self) -> TimerSetCall {
        TimerSetCall::new(Rc::clone(&self.variable))
    }

    pub fn get_set_type_call(&self) -> TimerSetTypeCall {
        TimerSetTypeCall::new(Rc::clone(&self.variable))
    }

    pub fn get_setup_call(&self) -> TimerSetupCall {
        TimerSetupCall::new(Rc::clone(&self.variable))
    }
}

impl ForeignObject for Timer {
    fn datatype(&self) -> DataType {
        DataType::Foreign(ForeignType::Timer)
    }
}

pub struct Event {
    variable: Rc<Variable>,
}

impl Event {
    pub fn new(variable: Rc<Variable>) -> Event {
        let event = Event { variable: Rc::clone(&variable) };
        variable.set_type(event.datatype());
        event
    }

    pub fn get_init_call(&self) -> EventInitCall {
        EventInitCall::new(Rc::clone(&self.variable))
    }

    pub fn get_add_payload_call(&self) -> EventAddPayloadCall {
        EventAddPayloadCall::new(Rc::clone(&self.variable))
    }

    pub fn get_deliver_call(&self) -> EventDeliverCall {
        EventDeliverCall::new(Rc::clone(&self.variable))
    }

    pub fn get_free_payload_call(&self) -> EventFreePayloadCall {
        EventFreePayloadCall::new(Rc::clone(&self.variable))
    }
}

impl ForeignObject for Event {
    fn datatype(&self) -> DataType {
        DataType::Foreign(ForeignType::Event)
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
        output.push(')');
        output
    }
}

// --------------- Protocol ---------------
pub struct ProtoCreateCall {
    state: Rc<StructDecl>,
}

impl ProtoCreateCall {
    pub fn new(state: Rc<StructDecl>) -> ProtoCreateCall {
        ProtoCreateCall { state }
    }
}

impl ForeignFunctionCall for ProtoCreateCall {
    fn get_name(&self) -> &str {
        "create_protocol_definition"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Int, DataType::String]//, DataType::Struct(Rc::clone(&self.state))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::Constant(Constant::Int(1)),
             Expression::Constant(Constant::String("Dummy".to_string()))] //, self.state]
    }

    fn return_type(&self) -> Option<DataType> {
        Some(DataType::Reference(Box::new(DataType::Foreign(ForeignType::ProtoDef))))
    }
}

pub struct ProtoAddMLoopCall {
    var: Rc<Variable>,
    main_loop: Rc<Function>,
}

impl ProtoAddMLoopCall {
    pub fn new(var: Rc<Variable>, main_loop: Rc<Function>) -> ProtoAddMLoopCall {
        ProtoAddMLoopCall { var, main_loop }
    }
}

impl ForeignFunctionCall for ProtoAddMLoopCall {
    fn get_name(&self) -> &str {
        "proto_def_add_protocol_main_loop"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::ProtoDef))),
             DataType::Foreign(ForeignType::ProtoDef)]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.var))),
            UnaryOperation::Ref)]
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
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
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
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        Some(DataType::Int)
    }
}

pub struct TimerInitCall {
    variable: Rc<Variable>,
}

impl TimerInitCall {
    pub fn new(variable: Rc<Variable>) -> TimerInitCall {
        TimerInitCall { variable }
    }
}

impl ForeignFunctionCall for TimerInitCall {
    fn get_name(&self) -> &str {
        "YggTimer_init"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Timer)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct TimerSetCall {
    variable: Rc<Variable>,
}

impl TimerSetCall {
    pub fn new(variable: Rc<Variable>) -> TimerSetCall {
        TimerSetCall { variable }
    }
}

impl ForeignFunctionCall for TimerSetCall {
    fn get_name(&self) -> &str {
        "YggTimer_set"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Timer)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct TimerSetTypeCall {
    variable: Rc<Variable>,
}

impl TimerSetTypeCall {
    pub fn new(variable: Rc<Variable>) -> TimerSetTypeCall {
        TimerSetTypeCall { variable }
    }
}

impl ForeignFunctionCall for TimerSetTypeCall {
    fn get_name(&self) -> &str {
        "YggTimer_setType"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Timer)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct TimerSetupCall {
    variable: Rc<Variable>,
}

impl TimerSetupCall {
    pub fn new(variable: Rc<Variable>) -> TimerSetupCall {
        TimerSetupCall { variable }
    }
}

impl ForeignFunctionCall for TimerSetupCall {
    fn get_name(&self) -> &str {
        "setupTimer"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Timer)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct EventInitCall {
    variable: Rc<Variable>,
}

impl EventInitCall {
    pub fn new(variable: Rc<Variable>) -> EventInitCall {
        EventInitCall { variable }
    }
}

impl ForeignFunctionCall for EventInitCall {
    fn get_name(&self) -> &str {
        "YggEvent_init"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Event)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct EventAddPayloadCall {
    variable: Rc<Variable>,
}

impl EventAddPayloadCall {
    pub fn new(variable: Rc<Variable>) -> EventAddPayloadCall {
        EventAddPayloadCall { variable }
    }
}

impl ForeignFunctionCall for EventAddPayloadCall {
    fn get_name(&self) -> &str {
        "YggEvent_addPayload"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Event)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct EventDeliverCall {
    variable: Rc<Variable>,
}

impl EventDeliverCall {
    pub fn new(variable: Rc<Variable>) -> EventDeliverCall {
        EventDeliverCall { variable }
    }
}

impl ForeignFunctionCall for EventDeliverCall {
    fn get_name(&self) -> &str {
        "deliverEvent"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Event)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}

pub struct EventFreePayloadCall {
    variable: Rc<Variable>,
}

impl EventFreePayloadCall {
    pub fn new(variable: Rc<Variable>) -> EventFreePayloadCall {
        EventFreePayloadCall { variable }
    }
}

impl ForeignFunctionCall for EventFreePayloadCall {
    fn get_name(&self) -> &str {
        "YggEvent_freePayload"
    }

    fn get_parameter_types(&self) -> Vec<DataType> {
        vec![DataType::Reference(Box::new(DataType::Foreign(ForeignType::Event)))]
    }

    fn get_args(&self) -> Vec<Expression> {
        vec![Expression::UnaryOperation(
            Box::new(Expression::Variable(Rc::clone(&self.variable))),
            UnaryOperation::Ref)]
    }

    fn return_type(&self) -> Option<DataType> {
        None
    }
}