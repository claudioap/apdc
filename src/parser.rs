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
use crate::yggl::program::{Program, Include};
use crate::yggl::environment::{Variable, Environment, Symbol};
use crate::yggl::function::*;
use crate::yggl::flow::{Conditional, Cycle};
use crate::yggl::structure::{StructDef, StructDecl, Attribute};

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

impl Program {
    pub fn from(pairs: Pairs<Rule>) -> Result<Program, CompilationError> {
        let mut program = Program::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::statement => {
                    let statement =
                        Statement::from(pair, program.get_env_mut())?;
                    program.add_statement(statement);
                }
                Rule::EOI => {}
                _ => unimplemented!()
            }
        }
        program.annotate();
        Ok(program)
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
                match lhs_pair.as_rule() {
                    Rule::identifier => {
                        let identifier = Statement::obtain_identifier(
                            lhs_pair, env, expression.data_type().unwrap())?;
                        match identifier {
                            Symbol::Variable(var) => {
                                Ok(Statement::Assignment(Rc::clone(&var), expression))
                            }
                            Symbol::Constant(_) | Symbol::Function(_) | Symbol::StructDecl(_) => {
                                Err(CompilationError::new(
                                    0, 0, "".to_string(),
                                    format!("Attribution to constant field.")))
                            }
                        }
                    }
                    Rule::attribute => {
                        let var_attr_pair = Statement::obtain_attribute(
                            lhs_pair, env, expression.data_type())?;
                        Ok(Statement::AttributeAssignment(var_attr_pair.0, var_attr_pair.1, expression))
                    }
                    _ => unreachable!("Unknown assignment LHS")
                }
            }
            Rule::function => {
                match lhs_pair.as_rule() {
                    Rule::identifier => {
                        let identifier_str = lhs_pair.as_str().to_string();
                        let function = Function::from(
                            rhs_pair,
                            identifier_str.to_string(),
                            env)?;
                        let function_rc = env.add_function(function);
                        Ok(Statement::FunctionDef(function_rc))
                    }
                    Rule::attribute => {
                        Err(CompilationError::new(
                            0, 0, "".to_string(),
                            format!("Direct function assignment (unimplemented).")))
                    }
                    _ => unreachable!("Unknown assignment LHS")
                }
            }
            Rule::struct_decl => {
                let identifier_str = lhs_pair.as_str().to_string();
                let struct_decl = StructDecl::from(rhs_pair, identifier_str.to_string())?;
                let struct_decl_rc = env.add_struct_decl(struct_decl);
                Ok(Statement::StructDecl(struct_decl_rc))
            }
            Rule::struct_def => {
                let struct_def = StructDef::from(rhs_pair, env)?;
                let struct_decl = struct_def.get_declaration();
                let struct_def_rc = Rc::new(struct_def);

                match lhs_pair.as_rule() {
                    Rule::identifier => {
                        let identifier = Statement::obtain_identifier(lhs_pair, env, DataType::Struct(struct_decl))?;
                        match identifier {
                            Symbol::Variable(var) => {
                                Ok(Statement::StructDef(Rc::clone(&var), struct_def_rc))
                            }
                            Symbol::Constant(_) | Symbol::Function(_) | Symbol::StructDecl(_) => {
                                Err(CompilationError::new(
                                    0, 0, "".to_string(),
                                    format!("Attribution to constant field.")))
                            }
                        }
                    }
                    Rule::attribute => {
                        unimplemented!("Direct structure assignment to attribute is not currenly implemented.");
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
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

    fn obtain_attribute(pair: Pair<Rule>, env: &mut Environment, dtype: Option<DataType>)
                        -> Result<(Rc<Variable>, Rc<Attribute>), CompilationError> {
        // TODO  validate data type
        let mut inner = pair.into_inner();
        let identifier_str = inner.next().unwrap().as_str();
        let attribute_str = inner.next().unwrap().as_str();

        if let Some(Symbol::Variable(var)) = env.get(identifier_str) {
            let variable = Rc::clone(&var);
            if let Some(DataType::Struct(decl)) = variable.data_type() {
                if let Some(attr) = decl.get_attribute(attribute_str) {
                    attr.set_data_type(dtype);
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
        } else {
            Err(CompilationError::new(
                0, 0, "".to_string(),
                format!("Attribution to attribute of non-struct {}", identifier_str)))
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
                            Some(Symbol::Constant(c)) => Ok(Expression::Constant(c.clone())),
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
                                        Ok(Expression::AttributeAccess(var, attr))
                                    } else {
                                        Err(CompilationError::new(
                                            0, 0, "".to_string(),
                                            format!("Unable to access \"{}\".\"{}\"", object_name, attr_name)))
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
    pub fn from(pair: Pair<Rule>, name: String, env: &mut Environment)
                -> Result<Function, CompilationError> {
        let mut parameters: Option<Vec<Rc<Variable>>> = Option::None;
        let mut statements = vec![];
        let mut function_env = Environment::new();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::parameters => {
                    parameters = Some(Function::read_parameters(pair, &mut function_env)?);
                }
                Rule::statement => {
                    let statement = Statement::from(pair, &mut function_env)?;
                    statements.push(statement);
                }
                _ => unreachable!()
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

        function_env.push_static(env.get_static_symbols());
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

    fn read_arguments(pair: Pair<Rule>, env: &Environment) -> Result<Vec<Expression>, CompilationError> {
        let mut arguments = vec!();
        for argument_pair in pair.into_inner() {
            let argument = Expression::from(argument_pair, env)?;
            arguments.push(argument);
        }
        Ok(arguments)
    }

    fn parse_call(pair: Pair<Rule>, env: &Environment) -> Result<Statement, CompilationError> {
        let mut inner_rules = pair.into_inner();
        let identifier = inner_rules.next().unwrap().as_str();
        let arguments = Function::read_arguments(inner_rules.next().unwrap(), env)?;
        if identifier == "print" {
            Ok(Statement::Print(arguments))
        } else {
            if let Some(symbol) = env.get(identifier) {
                if let Symbol::Function(function) = symbol {
                    Ok(Statement::Call(FunctionCall::new(Rc::clone(&function), arguments)))
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
    pub fn from(pair: Pair<Rule>, name: String) -> Result<StructDecl, CompilationError> {
        let mut attributes = vec!();
        for pair in pair.into_inner() {
            let attribute = Attribute::new(pair.as_str().to_string(), None);
            attributes.push(Rc::new(attribute));
        }
        Ok(StructDecl::new(name, attributes))
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