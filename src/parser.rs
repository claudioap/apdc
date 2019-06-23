use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use pest::Parser;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::error::Error;
use pest::error::LineColLocation::*;
use pest::prec_climber::*;
use crate::yggl::data::{Constant, DataType};
use crate::yggl::expression::{Expression, BinaryOperation};
use crate::yggl::statement::Statement;
use crate::yggl::environment::{Variable, Environment, Symbol, Label};
use crate::yggl::function::*;
use crate::yggl::flow::{Conditional, Cycle};
use crate::yggl::structure::{StructDef, StructDecl, Attribute, LocalAttribute};
use crate::yggl::protocol::{ProtocolDef, Interface, Handler, YggType, Include, Protocol};
use crate::yggl::timer::{TimerType, TimeUnit};
use crate::yggl::networking::Address;
use crate::yggl::foreign::{Message, Timer, Event, ListInitCall, ForeignFunctionCall};

#[derive(Parser)]
#[grammar = "yggl/grammar.pest"]
pub struct YGGLParser;

#[allow(dead_code)]
pub struct CompilationError {
    line: u32,
    col: u32,
    code: String,
    cause: String,
}

impl CompilationError {
    pub fn new(line: u32, col: u32, code: String, cause: String) -> CompilationError {
        CompilationError {
            line,
            col,
            code,
            cause,
        }
    }
}

impl fmt::Display for CompilationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} {}", self.line, self.col, self.cause)
    }
}

pub fn lex(code: &str) -> Result<Pairs<Rule>, &str> {
    match YGGLParser::parse(Rule::program, code) {
        Ok(program) => return Ok(program),
        Err(error) => {
            let Error {
                line_col: location,
                ..
            } = error;
            match location {
                Pos((line, col)) => {
                    print!("Unable to parse.\nLine {}, Col{}:", line, col);
                    let problematic_line = code.lines().nth(line - 1).unwrap();
                    println!("{:?}", problematic_line);
                    return Err("TODO");
                }
                _ => {}
            }
        }
    }
    unreachable!();
}

impl Protocol {
    pub fn from(pairs: Pairs<Rule>) -> Result<Protocol, CompilationError> {
        let mut protocol = Protocol::new();
        for pair in pairs {
            let env = protocol.get_env_mut();
            match pair.as_rule() {
                Rule::proto_def => {
                    let protocol_def = ProtocolDef::from(pair, env)?;
                    protocol.set_definition(protocol_def);
                }
                Rule::function => {
                    let function = Function::from(pair, env)?;
                    env.add_function(function);
                }
                Rule::struct_decl => {
                    let struct_decl = StructDecl::from(pair)?;
                    env.add_struct_decl(struct_decl);
                }
                Rule::constant_assignment => {
                    let mut inner = pair.into_inner();
                    let identifier = inner.next().unwrap().as_str();
                    let expression = Expression::from(inner.next().unwrap(), env)?;
                    env.set_defines(identifier, expression.eval(env));
                }
                Rule::EOI => {}
                _ => unimplemented!()
            }
        }
        protocol.compile()?;
        Ok(protocol)
    }
}

impl Statement {
    pub fn from(pair: Pair<Rule>, env: &mut Environment) -> Result<Statement, CompilationError> {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::assignment => {
                Statement::parse_assignment(pair, env)
            }
            Rule::function_call => {
                let call = Function::parse_call(pair, env);
                if let Ok(Statement::Print(_)) = call {
                    env.require_include(
                        Include::new("stdio.h".to_string(), true, None))
                }
                call
            }
            Rule::conditional_if => {
                let conditional = Conditional::from(pair, env)?;
                Ok(Statement::Conditional(conditional))
            }
            Rule::cycle_loop => {
                let cycle = Cycle::loop_from(pair, env)?;
                Ok(Statement::Cycle(cycle))
            }
            Rule::cycle_while => {
                let cycle = Cycle::while_from(pair, env)?;
                Ok(Statement::Cycle(cycle))
            }
            Rule::cycle_do_while => {
                let cycle = Cycle::do_while_from(pair, env)?;
                Ok(Statement::Cycle(cycle))
            }
            Rule::cycle_for => {
                let cycle = Cycle::for_from(pair, env)?;
                Ok(Statement::Cycle(cycle))
            }
            Rule::declaration => {
                let identifier = pair.as_str();
                Ok(Statement::Declaration(env.declare(identifier)))
            }
            Rule::attribute_call => {
                let mut inner = pair.into_inner();
                let (var, attribute) = get_attribute(inner.next().unwrap(), env)?;
                let arguments = read_arguments(inner.next().unwrap(), env)?;
                match attribute {
                    Attribute::Foreign(foreign) => {
                        let call = foreign.handle(var, arguments)?;
                        Ok(Statement::ForeignCall(call))
                    }
                    _ => Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("Attempted to call a non callable attribute {}", attribute.name())))
                }
            }
            Rule::function_return => {
                let expression = Expression::from(pair.into_inner().next().unwrap(), &env)?;
                Ok(Statement::Return(expression))
            }
            Rule::setup => {
                let mut inner = pair.into_inner();
                let ttype = match inner.next().unwrap().as_str() {
                    "periodic" => {
                        TimerType::Periodic
                    }
                    _ => unimplemented!()
                };
                let identifier = inner.next().unwrap().as_str();
                let _var = match env.get(identifier) {
                    Some(Symbol::Variable(var)) => var,
                    Some(_) => return Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("{} type mismatch", identifier))),
                    None =>
                        env.declare(identifier)
                };

                let aux_var = env.declare_aux();
                aux_var.set_declared();
                let wait = TimeUnit::read_value(inner.next().unwrap());
                let period = TimeUnit::read_value(inner.next().unwrap());
                let timer = Timer::new(Rc::clone(&aux_var), ttype, wait, period);
                let statements = vec![
                    Statement::Declaration(Rc::clone(&aux_var)),
                    Statement::ForeignCall(Box::new(timer.get_init_call())),
                    Statement::ForeignCall(Box::new(timer.get_set_call())),
                    Statement::ForeignCall(Box::new(timer.get_set_type_call())),
                    Statement::ForeignCall(Box::new(timer.get_setup_call()))];
                Ok(Statement::Composite(statements))
            }
            Rule::notify => {
                let mut inner = pair.into_inner();
                let identifier = inner.next().unwrap().as_str();
                let _var = match env.get(identifier) {
                    Some(Symbol::Variable(var)) => var,
                    Some(_) => return Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("{} type mismatch", identifier))),
                    None =>
                        env.declare(identifier)
                };

                let _arguments = read_arguments(inner.next().unwrap(), env)?;
//                let struct_decl = match env.get(identifier) {
//                    Some(Symbol::StructDecl(decl)) => decl,
//                    Some(_) => return Err(CompilationError::new(
//                        0, 0, "".to_string(),
//                        format!("{} type mismatch", identifier))),
//                    None =>
//                        return Err(CompilationError::new(
//                            0, 0, "".to_string(),
//                            format!("{} not found", identifier)))
//                };
//                let notif_definition = StructDef::new(struct_decl, HashMap::new());
                let aux_var = env.declare_aux();
                aux_var.set_declared();
                let event = Event::new(Rc::clone(&aux_var));
                let statements = vec![
                    Statement::Declaration(Rc::clone(&aux_var)),
                    Statement::ForeignCall(Box::new(event.get_init_call())),
                    Statement::ForeignCall(Box::new(event.get_deliver_call())),
                    Statement::ForeignCall(Box::new(event.get_free_payload_call()))];
                Ok(Statement::Composite(statements))
            }
            Rule::send => {
                let mut inner = pair.into_inner();
                let identifier = inner.next().unwrap().as_str();
                let _var = match env.get(identifier) {
                    Some(Symbol::Variable(var)) => var,
                    Some(_) => return Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("{} type mismatch", identifier))),
                    None =>
                        env.declare(identifier)
                };
                let aux_var = env.declare_aux();
                let message = Message::new(Rc::clone(&aux_var));
                let address = Address::from(inner.next().unwrap())?;
                let _arguments = read_arguments(inner.next().unwrap(), env)?;
                let statements = vec![
                    Statement::Declaration(Rc::clone(&aux_var)),
                    Statement::ForeignCall(Box::new(message.get_init_call(address))),
                    Statement::ForeignCall(Box::new(message.get_dispatch_call()))];
                Ok(Statement::Composite(statements))
            }
            _ => unreachable!("{}", pair)
        }
    }

    fn parse_assignment(pair: Pair<Rule>, env: &mut Environment) -> Result<Statement, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let lhs_pair = inner_rules.next().unwrap();
        let rhs_pair = inner_rules.next().unwrap();
        match rhs_pair.as_rule() {
            Rule::expression => {
                let expression = Expression::from(rhs_pair, env)?;
                if expression.data_type().is_none() {
                    return Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("Expression {} has no return.", expression.transpile())));
                }
                match lhs_pair.as_rule() {
                    Rule::identifier => {
                        let identifier = Statement::obtain_identifier(
                            lhs_pair, env, expression.data_type().unwrap())?;
                        match identifier {
                            Symbol::Variable(var) => {
                                Ok(Statement::Assignment(Rc::clone(&var), expression))
                            }
                            _ => {
                                Err(CompilationError::new(
                                    0, 0, "".to_string(),
                                    format!("Attribution to constant field.")))
                            }
                        }
                    }
                    Rule::attribute => {
                        let (var, attribute) = get_attribute(lhs_pair, env)?;
                        if let Err(message) = attribute.set_data_type(expression.data_type()) {
                            return Err(CompilationError::new(0, 0, "".to_string(), message));
                        }
                        match attribute {
                            Attribute::Local(local) =>
                                Ok(Statement::AttributeAssignment(var, local, expression)),
                            Attribute::Foreign(foreign) =>
                                Ok(Statement::ForeignCall(foreign.handle(var, vec![expression])?)),
                        }
                    }
                    _ => unreachable!("Unknown assignment LHS")
                }
            }
            Rule::struct_def => {
                let struct_def = StructDef::from(rhs_pair, env)?;
                let struct_decl = struct_def.get_declaration();
                let struct_def_rc = Rc::new(struct_def);

                match lhs_pair.as_rule() {
                    Rule::identifier => {
                        let identifier = Statement::obtain_identifier(
                            lhs_pair, env, DataType::Struct(struct_decl))?;
                        match identifier {
                            Symbol::Variable(var) => {
                                Ok(Statement::StructDef(Rc::clone(&var), struct_def_rc))
                            }
                            _ => {
                                Err(CompilationError::new(
                                    0, 0, "".to_string(),
                                    format!("Attribution to constant field.")))
                            }
                        }
                    }
                    Rule::attribute => {
                        unimplemented!("Direct structure assignment to attribute is not currently implemented.");
                    }
                    _ => unreachable!()
                }
            }
            Rule::list_init => {
                let list_decl = env.get("list");
                let init_call = Box::new(ListInitCall {});

                if let Some(Symbol::StructDecl(list)) = list_decl {
                    match lhs_pair.as_rule() {
                        Rule::identifier => {
                            let identifier = Statement::obtain_identifier(
                                lhs_pair, env,
                                DataType::Reference(Box::new(
                                    DataType::Struct(Rc::clone(&list)))))?;
                            match identifier {
                                Symbol::Variable(var) => {
                                    var.set_type(init_call.return_type().unwrap());
                                    let statement = Statement::Composite(vec![
                                        Statement::Assignment(Rc::clone(&var), Expression::Foreign(init_call))
                                    ]);
                                    Ok(statement)
                                }
                                _ => {
                                    Err(CompilationError::new(
                                        0, 0, "".to_string(),
                                        format!("Attribution to constant field.")))
                                }
                            }
                        }
                        Rule::attribute => {
                            let (var, attrib) = get_attribute(lhs_pair, env)?;
                            let dtype = Some(DataType::Reference(Box::new(
                                DataType::Struct(Rc::clone(&list)))));
                            if let Err(message) = attrib.set_data_type(dtype) {
                                return Err(CompilationError::new(
                                    0, 0, "".to_string(), message));
                            }
                            let statements = match attrib {
                                Attribute::Local(local) => vec![
                                    Statement::AttributeAssignment(
                                        var, local, Expression::Foreign(init_call))],
                                Attribute::Foreign(_) => unimplemented!(),
                            };
                            let statement = Statement::Composite(statements);
                            Ok(statement)
                        }
                        _ => unreachable!()
                    }
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!("{}", rhs_pair)
        }
    }

    fn obtain_identifier(pair: Pair<Rule>, env: &mut Environment, expected_dtype: DataType)
                         -> Result<(Symbol), CompilationError> {
        // TODO  validate data type
        let identifier = pair.as_str();

        let var = match env.get(identifier) {
            Some(sym) => {
                if let Symbol::Variable(v) = sym {
                    v
                } else {
                    return Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("Redeclaring "),
                    ));
                }
            }
            None => env.declare(identifier),
        };
        var.set_type(expected_dtype);

        if let Some(symbol) = env.get(identifier) {
            Ok(symbol.clone())
        } else {
            unreachable!();
        }
    }
}

impl Cycle {
    pub fn loop_from(pair: Pair<Rule>, env: &mut Environment) -> Result<Cycle, CompilationError> {
        let statements =
            Cycle::read_statements(pair.into_inner().next().unwrap(), env)?;
        Ok(Cycle::Loop(statements))
    }

    pub fn while_from(pair: Pair<Rule>, env: &mut Environment) -> Result<Cycle, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let pair = inner_rules.next().unwrap();
        let condition = Expression::from(pair, env)?;
        let statements =
            Cycle::read_statements(inner_rules.next().unwrap(), env)?;
        Ok(Cycle::While(condition, statements))
    }

    pub fn do_while_from(pair: Pair<Rule>, env: &mut Environment) -> Result<Cycle, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let statements =
            Cycle::read_statements(inner_rules.next().unwrap(), env)?;
        let pair = inner_rules.next().unwrap();
        let condition = Expression::from(pair, env)?;
        Ok(Cycle::DoWhile(condition, statements))
    }

    pub fn for_from(_pair: Pair<Rule>, _env: &mut Environment) -> Result<Cycle, CompilationError> {
        unimplemented!("For loops unimplemented yet");
    }

    fn read_statements(body: Pair<Rule>, env: &mut Environment)
                       -> Result<Vec<Box<Statement>>, CompilationError> {
        let mut statements = vec!();
        for pair in body.into_inner() {
            let statement = Statement::from(pair, env)?;
            statements.push(Box::new(statement));
        }
        Ok(statements)
    }
}

// Define the precedence climber for an Expression.
// This tells a parser which operations to parse first in an unparsed expression.
lazy_static! {
    static ref BINOP_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        PrecClimber::new(vec![
            Operator::new(or, Left),
            Operator::new(and, Left),
            Operator::new(eq, Left) | Operator::new(neq, Left),
            Operator::new(le, Left) | Operator::new(gr, Left) |
                Operator::new(leq, Left) | Operator::new(geq, Left),
            Operator::new(add, Left) | Operator::new(sub, Left),
            Operator::new(mul, Left) | Operator::new(div, Left)| Operator::new(rem, Left),
            Operator::new(pow, Right)
        ])
    };
}

impl Expression {
    pub fn from(pair: Pair<Rule>, env: &Environment) -> Result<Expression, CompilationError> {
        let pairs = pair.into_inner();
        BINOP_CLIMBER.climb(
            pairs,
            |pair: Pair<Rule>|
                match pair.as_rule() {
                    Rule::expression => Expression::from(pair, env),
                    Rule::number => //TODO Trait std::str::FromStr
                        Ok(Expression::Constant(Constant::parse_number(pair.as_str()))),
                    Rule::boolean =>
                        Ok(Expression::Constant(Constant::Bool(pair.as_str() == "true"))),
                    Rule::string => {
                        let inner = pair.into_inner().next().unwrap();
                        Ok(Expression::Constant(Constant::String(inner.as_str().to_string())))
                    }
                    Rule::identifier => {
                        match env.get(pair.as_str()) {
                            Some(Symbol::Define(c)) => Ok(Expression::Constant(c.value())),
                            Some(Symbol::Variable(var)) => Ok(Expression::Variable(var.clone())),
                            _ => Err(CompilationError::new(
                                0, 0, "".to_string(),
                                format!("Failed to resolve symbol {}", pair.as_str())))
                        }
                    }
                    Rule::attribute => {
                        let mut inner = pair.into_inner();
                        let object_name = inner.next().unwrap().as_str();
                        let attr_name = inner.next().unwrap().as_str();
                        if let Some(symbol) = env.get(object_name) {
                            let var = match symbol {
                                Symbol::Variable(var) => var,
                                _ => return Err(
                                    CompilationError::new(
                                        0, 0, "".to_string(),
                                        format!("\"{}\" is not an object", object_name)))
                            };


                            match var.data_type() {
                                Some(DataType::Struct(struct_decl)) => {
                                    if let Some(attr) = struct_decl.get_attribute(attr_name) {
                                        match attr {
                                            Attribute::Local(local) =>
                                                Ok(Expression::AttributeAccess(var, local)),
                                            Attribute::Foreign(foreign) => {
                                                Ok(Expression::Foreign(
                                                    foreign.handle(var, vec![])?))
                                            }
                                        }
                                    } else {
                                        Err(CompilationError::new(
                                            0, 0, "".to_string(),
                                            format!("Unable to access \"{}\".\"{}\"",
                                                    object_name, attr_name)))
                                    }
                                }
                                _ =>
                                    Err(CompilationError::new(
                                        0, 0, "".to_string(),
                                        format!("\"{}\" is not an object", object_name)))
                            }
                        } else {
                            Err(CompilationError::new(
                                0, 0, "".to_string(),
                                format!("\"{}\" is not declared", object_name)))
                        }
                    }
                    Rule::function_call => {
                        if let Statement::Call(call) = Function::parse_call(pair, env)? {
                            Ok(Expression::Call(call))
                        } else {
                            Err(CompilationError::new(
                                0, 0, "".to_string(),
                                "Unable to parse function call".to_string()))
                        }
                    }
                    _ => unreachable!("{}", pair),
                },
            |lhs: Result<Expression, CompilationError>, op: Pair<Rule>, rhs: Result<Expression, CompilationError>|
                {
                    let l = lhs?;
                    let r = rhs?;
                    match op.as_rule() {
                        Rule::add => Ok(l + r),
                        Rule::sub => Ok(l - r),
                        Rule::mul => Ok(l * r),
                        Rule::div => Ok(l / r),
                        Rule::pow => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Pow,
                            Box::new(r))),
                        Rule::eq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Eq,
                            Box::new(r))),
                        Rule::neq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Neq,
                            Box::new(r))),
                        Rule::gr => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Gr,
                            Box::new(r))),
                        Rule::geq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Geq,
                            Box::new(r))),
                        Rule::le => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Le,
                            Box::new(r))),
                        Rule::leq => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Leq,
                            Box::new(r))),
                        Rule::and => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::And,
                            Box::new(r))),
                        Rule::or => Ok(Expression::BinaryOperation(
                            Box::new(l),
                            BinaryOperation::Or,
                            Box::new(r))),
                        _ => unreachable!(),
                    }
                },
        )
    }
}

impl DataType {
    pub fn from(pair: Pair<Rule>, env: &Environment) -> Result<DataType, CompilationError> {
        match pair.as_rule() {
            Rule::int_t => {
                Ok(DataType::Int)
            }
            Rule::float_t => {
                Ok(DataType::Float)
            }
            Rule::char_t => {
                Ok(DataType::Char)
            }
            Rule::bool_t => {
                Ok(DataType::Bool)
            }
            Rule::fun_t => {
                Ok(DataType::Function)
            }
            Rule::identifier => {
                let identifier = pair.as_str();
                if let Some(symbol) = env.get(identifier) {
                    if let Symbol::StructDecl(decl) = symbol {
                        Ok(DataType::Struct(decl))
                    } else {
                        Err(CompilationError::new(
                            0, 0, "".to_string(),
                            format!("{} is not a data type.", identifier),
                        ))
                    }
                } else {
                    Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("Unknown data type {}.", identifier),
                    ))
                }
            }
            _ => unreachable!()
        }
    }
}

impl Conditional {
    pub fn from(pair: Pair<Rule>, env: &mut Environment)
                -> Result<Conditional, CompilationError> {
        let mut pairs = pair.into_inner();
        let mut condition_pair = pairs.next().unwrap();
        let mut condition = Expression::from(condition_pair, env)?;
        let mut body = pairs.next().unwrap();
        let mut statements: Vec<Statement> = vec!();
        for statement_pair in body.into_inner() {
            let statement = Statement::from(statement_pair, env)?;
            statements.push(statement);
        }
        let mut conditional = Conditional::new(Some(condition), statements);
        loop {
            let next = pairs.next();
            if let Some(pair) = next {
                match pair.as_rule() {
                    Rule::conditional_elif => {
                        pairs = pair.into_inner();
                        condition_pair = pairs.next().unwrap();
                        condition = Expression::from(condition_pair, env)?;
                        body = pairs.next().unwrap();
                        let mut statements: Vec<Statement> = vec!();
                        for statement_pair in body.into_inner() {
                            let statement = Statement::from(statement_pair, env)?;
                            statements.push(statement);
                        }
                        conditional.add_sibling(Some(condition), statements);
                    }
                    Rule::conditional_else => {
                        pairs = pair.into_inner();
                        body = pairs.next().unwrap();
                        let mut statements: Vec<Statement> = vec!();
                        for statement_pair in body.into_inner() {
                            let statement = Statement::from(statement_pair, env)?;
                            statements.push(statement);
                        }
                        conditional.add_sibling(None, statements);
                    }
                    _ => unreachable!()
                }
            } else {
                break;
            }
        }

        Ok(conditional)
    }
}

impl Function {
    pub fn from(pair: Pair<Rule>, env: &mut Environment) -> Result<Function, CompilationError> {
        let mut parameters: Option<Vec<Rc<Variable>>> = Option::None;
        let mut function_env = Environment::new();
        function_env.push_static(env.get_static_symbols());
        let mut inner = pair.into_inner();
        let mut pair = inner.next().unwrap();
        if pair.as_rule() != Rule::identifier {
            panic!()
        }
        let name = pair.as_str().to_string();
        pair = inner.next().unwrap();
        if pair.as_rule() == Rule::parameters {
            parameters = Some(Function::read_parameters(pair, &mut function_env)?);
            pair = inner.next().unwrap();
        }
        let statements = Function::read_body(pair, &mut function_env)?;

        if let Some(parameters_vec) = parameters {
            Function::new(function_env, name, parameters_vec, statements)
        } else {
            Function::new(function_env, name, vec!(), statements)
        }
    }

    pub fn anonymous_from(pair: Pair<Rule>, env: &mut Environment, name: String)
                          -> Result<Function, CompilationError> {
        let mut parameters: Option<Vec<Rc<Variable>>> = Option::None;
        let mut function_env = Environment::new();
        function_env.push_static(env.get_static_symbols());
        let mut inner = pair.into_inner();
        let first_pair = inner.next().unwrap();
        let body = if first_pair.as_rule() == Rule::parameters {
            parameters = Some(Function::read_parameters(first_pair, &mut function_env)?);
            inner.next().unwrap()
        } else {
            first_pair
        };
        let statements = Function::read_body(body, &mut function_env)?;

        if let Some(parameters_vec) = parameters {
            Function::new(function_env, name, parameters_vec, statements)
        } else {
            Function::new(function_env, name, vec!(), statements)
        }
    }

    fn read_parameters(pair: Pair<Rule>, env: &mut Environment)
                       -> Result<Vec<Rc<Variable>>, CompilationError> {
        let mut parameters = vec!();
        for parameter_pair in pair.into_inner() {
            match parameter_pair.as_rule() {
                Rule::identifier => {
                    let parameter = env.declare(parameter_pair.as_str());
                    parameter.set_declared();
                    parameters.push(parameter);
                }
                Rule::cast => {
                    let mut inner = parameter_pair.into_inner();
                    let identifier = inner.next().unwrap().as_str();
                    let dtype = DataType::from(inner.next().unwrap(), env)?;
                    let parameter = env.declare(identifier);
                    parameter.set_type(dtype);
                    parameter.set_declared();
                    parameters.push(parameter);
                }
                _ => unreachable!()
            }
        }
        Ok(parameters)
    }

    fn read_body(pair: Pair<Rule>, mut function_env: &mut Environment)
                 -> Result<Vec<Statement>, CompilationError> {
        let mut statements = vec![];
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::statement => {
                    let statement = Statement::from(pair, &mut function_env)?;
                    statements.push(statement);
                }
                _ => unreachable!(format!("{}", pair))
            }
        }
        if statements.is_empty() {
            return Err(
                CompilationError::new(
                    0,
                    0,
                    "".to_string(),
                    "Function definition without statements".to_string()));
        }
        Ok(statements)
    }

    pub fn parse_call(pair: Pair<Rule>, env: &Environment) -> Result<Statement, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let identifier = inner_rules.next().unwrap().as_str();
        let arguments = read_arguments(inner_rules.next().unwrap(), env)?;
        if identifier == "print" {
            Ok(Statement::Print(arguments))
        } else {
            if let Some(symbol) = env.get(identifier) {
                if let Symbol::Function(function) = symbol {
                    let mut arg_index = 0;
                    for argument in &*arguments {
                        function.set_parameter_type(arg_index, argument.data_type().unwrap())?;
                        arg_index += 1;
                    }
                    Ok(Statement::Call(
                        FunctionCall::new(Rc::clone(&function), arguments)))
                } else {
                    Err(CompilationError::new(
                        0, 0, "".to_string(),
                        format!("Identifier {} isn't a function", { identifier })))
                }
            } else {
                Err(CompilationError::new(
                    0, 0, "".to_string(),
                    format!("Identifier {} isn't defined.", { identifier })))
            }
        }
    }
}

impl StructDecl {
    pub fn from(pair: Pair<Rule>) -> Result<StructDecl, CompilationError> {
        let mut inner = pair.into_inner();
        let first_pair = inner.next().unwrap();
        if first_pair.as_rule() == Rule::identifier {
            let name = first_pair.as_str().to_string();
            let attributes = StructDecl::read_body(inner.next().unwrap());
            Ok(StructDecl::new(name, attributes))
        } else {
            panic!();
        }
    }

    pub fn anonymous_from(pair: Pair<Rule>, name: String) -> Result<StructDecl, CompilationError> {
        let mut inner = pair.into_inner();
        let first_pair = inner.next().unwrap();
        if first_pair.as_rule() == Rule::struct_decl_body {
            let attributes = StructDecl::read_body(first_pair);
            Ok(StructDecl::new(name, attributes))
        } else {
            panic!();
        }
    }

    fn read_body(pair: Pair<Rule>) -> Vec<Attribute> {
        let mut attributes = vec!();
        for pair in pair.into_inner() {
            let attribute = Attribute::Local(
                Rc::new(LocalAttribute::new(pair.as_str().to_string(), None)));
            attributes.push(attribute);
        }
        attributes
    }
}

impl StructDef {
    pub fn from(pair: Pair<Rule>, env: &Environment) -> Result<StructDef, CompilationError> {
        let mut inner = pair.into_inner();
        let identifier = inner.next().unwrap().as_str();
        match env.get(identifier) {
            Some(Symbol::StructDecl(declaration)) => {
                // TODO attribute initialization
                Ok(StructDef::new(declaration, HashMap::new()))
            }
            _ => Err(CompilationError::new(
                0, 0,
                "".to_string(),
                format!("Attempted to build a struct from non-struct symbol {}", identifier)))
        }
    }
}

impl ProtocolDef {
    pub fn from(pair: Pair<Rule>, env: &mut Environment) -> Result<ProtocolDef, CompilationError> {
        let mut inner = pair.into_inner();
        let name = inner.next().unwrap().as_str().to_string();
        let mut proto_id: Option<u32> = None;
        let mut state_decl: Option<Rc<StructDecl>> = None;
        let mut init_func: Option<Rc<Function>> = None;
        let mut loop_func: Option<Rc<Function>> = None;
        let mut interfaces: Vec<Interface> = vec![];
        let mut handlers: Vec<Handler> = vec![];
        for pair in inner {
            match pair.as_rule() {
                Rule::proto_id => {
                    let id = pair.into_inner().next().unwrap().as_str().parse::<u32>().unwrap();
                    proto_id = Some(id);
                }
                Rule::proto_state => {
                    let mut struct_decl =
                        StructDecl::anonymous_from(
                            pair.into_inner().next().unwrap(),
                            format!("{}_proto_state", name))?;
                    struct_decl.set_exported();
                    state_decl = Some(env.add_struct_decl(struct_decl));
                }
                Rule::proto_init => {
                    let init = Function::anonymous_from(
                        pair.into_inner().next().unwrap(),
                        env,
                        format!("{}_proto_init", name))?;
                    init_func = Some(env.add_function(init));
                }
                Rule::proto_loop => {
                    let init = Function::anonymous_from(
                        pair.into_inner().next().unwrap(),
                        env,
                        format!("{}_proto_loop", name))?;
                    loop_func = Some(env.add_function(init));
                }
                Rule::proto_iface => {
                    let mut new_interfaces = ProtocolDef::read_interfaces(pair, env)?;
                    interfaces.append(&mut new_interfaces);
                }
                Rule::proto_handler => {
                    let handler = Handler::from(&name, pair, env)?;
                    handlers.push(handler);
                }
                _ => unreachable!()
            }
        }
        if state_decl.is_none() || init_func.is_none() {
            return Err(CompilationError::new(
                0, 0, "".to_string(),
                "Protocol declaration without a state or initialization.".to_string()));
        }

        let mut protocol = ProtocolDef::new(
            proto_id.unwrap(),
            name,
            state_decl.unwrap(),
            init_func.unwrap());

        if let Some(func) = loop_func {
            protocol.add_main_loop(func);
        }
        protocol.add_handlers(handlers);


        Ok(protocol)
    }

    pub fn read_interfaces(pair: Pair<Rule>, env: &mut Environment)
                           -> Result<Vec<Interface>, CompilationError> {
        let mut interfaces = vec![];
        for pair in pair.into_inner() {
            interfaces.push(Interface::from(pair, env)?);
        }
        Ok(interfaces)
    }

    pub fn read_ygg_type(pair: Pair<Rule>) -> YggType {
        match pair.as_rule() {
            Rule::request => {
                YggType::Request
            }
            Rule::reply => {
                YggType::Reply
            }
            Rule::message => {
                YggType::Message
            }
            Rule::timer => {
                YggType::Timer
            }
            Rule::notification => {
                YggType::Notification
            }
            _ => unreachable!("{}", pair)
        }
    }
}


impl Interface {
    pub fn from(pair: Pair<Rule>, env: &mut Environment) -> Result<Interface, CompilationError> {
        match pair.as_rule() {
            Rule::proto_iface_producer => {
                let mut inner = pair.into_inner();
                let ytype = ProtocolDef::read_ygg_type(inner.next().unwrap());
                let identifier = inner.next().unwrap().as_str();
                match env.get(identifier) {
                    Some(Symbol::Variable(var)) => {
                        Ok(Interface::Producer(ytype, var, 1, 1))
                    }
                    Some(_) => {
                        Err(CompilationError::new(
                            0, 0, "".to_string(),
                            "TODO Undefined".to_string()))
                    }
                    None => {
                        let var = env.declare(identifier);
                        Ok(Interface::Producer(ytype, var, 1, 1))
                    }
                }
            }
            Rule::proto_iface_consumer => {
                let mut inner = pair.into_inner();
                let ytype = ProtocolDef::read_ygg_type(inner.next().unwrap());
                let identifier = inner.next().unwrap().as_str();
                match env.get(identifier) {
                    Some(Symbol::Variable(var)) => {
                        Ok(Interface::Consumer(ytype, var, 1))
                    }
                    Some(_) => {
                        Err(CompilationError::new(
                            0, 0, "".to_string(),
                            "TODO Undefined".to_string()))
                    }
                    None => {
                        let var = env.declare(identifier);
                        Ok(Interface::Consumer(ytype, var, 1))
                    }
                }
            }
            _ => unreachable!()
        }
    }
}

impl Handler {
    pub fn from(proto_name: &str, pair: Pair<Rule>, env: &mut Environment) -> Result<Handler, CompilationError> {
        let mut inner = pair.into_inner();
        let ytype = ProtocolDef::read_ygg_type(inner.next().unwrap());
        let identifier = inner.next().unwrap().as_str();
        let label = match env.get(identifier) {
            Some(Symbol::Label(label)) => {
                if label.get_type() == ytype {
                    label
                } else {
                    unreachable!(format!("Label {} is used for different types", identifier))
                }
            }
            None =>
                env.add_label(Label::new(identifier.to_string(), ytype.clone())),
            _ => return Err(CompilationError::new(
                0, 0, "".to_string(),
                format!("The label {} is being reused", identifier)))
        };
        let function = Function::anonymous_from(
            inner.next().unwrap(), env,
            format!("{}_{}_{}_handler", proto_name, ytype, identifier))?;
        let function_rc = env.add_function(function);
        Ok(Handler::new(label, function_rc))
    }
}

impl TimeUnit {
    pub fn read_value(pair: Pair<Rule>) -> u64 {
        if pair.as_str() == "0" {
            return 0;
        }
        let mut value = 0u64;
        let mut value_part = 0u64;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::positive_integer => {
                    value_part = pair.as_str().parse::<u64>().unwrap();
                    continue;
                }
                Rule::time_unit => {
                    let unit = match pair.into_inner().next().unwrap().as_rule() {
                        Rule::hour => TimeUnit::Hour,
                        Rule::minute => TimeUnit::Minute,
                        Rule::second => TimeUnit::Second,
                        Rule::milli => TimeUnit::Milli,
                        _ => unreachable!()
                    };
                    value += unit.scale(value_part);
                }
                _ => unreachable!()
            }
        }
        value
    }
}

impl Address {
    pub fn from(pair: Pair<Rule>) -> Result<Address, CompilationError> {
        match pair.as_rule() {
            Rule::broadcast_address => Ok(Address::Broadcast),
            _ => unimplemented!()
        }
    }
}


fn read_arguments(pair: Pair<Rule>, env: &Environment) -> Result<Vec<Expression>, CompilationError> {
    let mut arguments = vec!();
    for argument_pair in pair.into_inner() {
        let argument = Expression::from(argument_pair, env)?;
        arguments.push(argument);
    }
    Ok(arguments)
}


fn get_attribute(pair: Pair<Rule>, env: &mut Environment)
                 -> Result<(Rc<Variable>, Attribute), CompilationError> {
    let mut inner = pair.into_inner();
    let identifier_str = inner.next().unwrap().as_str();
    let attribute_str = inner.next().unwrap().as_str();

    if let Some(Symbol::Variable(var)) = env.get(identifier_str) {
        let variable = Rc::clone(&var);
        let decl = if let Some(dtype) = var.data_type() {
            if let Some(decl) = get_struct_decl_from_type(&dtype, env) {
                decl
            } else {
                return Err(CompilationError::new(
                    0, 0, "".to_string(),
                    format!("Access to attribute of non-struct {}", identifier_str)));
            }
        } else {
            return Err(CompilationError::new(
                0, 0, "".to_string(),
                format!("Access to attribute of unknown {}", identifier_str)));
        };

        if let Some(attr) = decl.get_attribute(attribute_str) {
            Ok((variable, attr))
        } else {
            Err(CompilationError::new(
                0, 0, "".to_string(),
                format!("Attribute {} not found", attribute_str)))
        }
    } else {
        Err(CompilationError::new(
            0, 0, "".to_string(),
            format!("Attribution to attribute of non-struct {}", identifier_str)))
    }
}

fn get_struct_decl_from_type(dtype: &DataType, env: &Environment) -> Option<Rc<StructDecl>> {
    match dtype {
        DataType::Struct(decl) => Some(Rc::clone(decl)),
        DataType::Reference(idtype) => get_struct_decl_from_type(idtype, env),
        DataType::Foreign(foreign) => {
            if let Some(Symbol::StructDecl(decl)) = env.get(foreign.name()) {
                Some(decl)
            } else {
                None
            }
        }
        _ => None
    }
}