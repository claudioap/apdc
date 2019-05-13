use std::collections::LinkedList;
use std::rc::Rc;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::data::{DataType, Evaluable, Constant};
use crate::yggl::expression::Expression;
use crate::parser::CompilationError;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
pub struct Function {
    name: String,
    parameters: Vec<Rc<Variable>>,
    environment: Environment,
    statements: LinkedList<Statement>,
    return_type: Option<DataType>,
}

impl Function {
    pub fn new(environment: Environment, name: String, parameters: Vec<Rc<Variable>>,
               statements: LinkedList<Statement>) -> Result<Function, CompilationError> {
        let dtype = Function::determine_return(&statements)?;
        Ok(Function {
            name,
            parameters,
            environment,
            statements,
            return_type: dtype,
        })
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    fn determine_return(statements: &LinkedList<Statement>) -> Result<Option<DataType>, CompilationError> {
        let mut dtype: Option<DataType> = None;
        for statement in statements {
            if let Statement::Return(ref evaluable) = statement {
                if dtype == None {
                    dtype = evaluable.data_type();
                } else if dtype != evaluable.data_type() {
                    return Err(
                        CompilationError::new(
                            0, 0, "".to_string(),
                            "Function returns two different data types".to_string()));
                }
            }
        }
        Ok(None)
    }

    pub fn transpile(&self) -> String {
        let rtype = match &self.return_type {
            Some(dtype) => dtype.transpile(),
            _ => "void"
        };
        let mut result = String::new();
        result.reserve(1024 * 10); // Reserve 10 KB to prevent further allocations
        result.push_str(format!("{} {}(", rtype, self.name).as_str());
        for parameter in &self.parameters {
            let dtype = parameter.get_type().expect("Parameter type unknown");
            let identifier = parameter.get_identifier();
            result.push_str(dtype.transpile());
            result.push(' ');
            result.push_str(identifier);
            result.push(',');
        }
        if self.parameters.is_empty(){
            result.push_str("){\n");
        }else{
            result.pop();// Remove last comma
            result.push_str("){\n");
        }
        for statement in &self.statements {
            result.push_str(format!(
                "    {}\n",
                statement.transpile(&self.environment)).as_str());
        }
        result.push_str("}\n");
        result
    }

    pub fn data_type(&self) -> Option<DataType> {
        self.return_type.clone()
    }

    pub fn call(&self) -> Option<Constant> {
        None
    }
}

#[allow(dead_code)]
pub struct FunctionCall {
    function: Rc<Function>,
    arguments: Vec<Expression>,
}

#[allow(dead_code)]
impl FunctionCall {
    pub fn new(function: Rc<Function>, arguments: Vec<Expression>) -> FunctionCall {
        FunctionCall { function, arguments }
    }

    pub fn get_function(&self) -> Rc<Function> {
        return Rc::clone(&self.function);
    }

    fn data_type(&self) -> Option<DataType> {
        self.function.data_type()
    }

    fn eval(&self) -> Option<Constant> {
        self.function.call()
    }
}