use std::collections::{HashMap, LinkedList};
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::language::Program;
use crate::yggl::data::{DataType, Evaluable, Constant};
use crate::parser::CompilationError;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
#[allow(dead_code)]
pub struct Function {
    name: String,
    parameters: Vec<Variable>,
    environment: Environment,
    static_vars: HashMap<String, Variable>,
    statements: LinkedList<Statement>,
    return_type: Option<DataType>,
}

#[allow(dead_code)]
impl Function {
    pub fn new(name: String, parameters: Vec<Variable>, statements: LinkedList<Statement>) -> Result<Function, CompilationError> {
        let dtype = Function::determine_return(&statements)?;
        Ok(Function {
            name,
            parameters,
            environment: Environment::new(),
            static_vars: HashMap::new(),
            statements,
            return_type: dtype,
        })
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

    pub fn transpile(&self, program: &Program) -> String {
        let rtype = match &self.return_type {
            Some(dtype) => dtype.transpile(),
            _ => "void"
        };
        let mut result = String::new();
        result.reserve(1024 * 10); // Reserve 10 KB to prevent further allocations
        result.push_str(format!("{} {}((", rtype, self.name).as_str());
        for parameter in &self.parameters {
            let dtype = parameter.get_type().expect("Parameter type unknown");
            let identifier = parameter.get_identifier();
            result.push_str(dtype.transpile());
            result.push(' ');
            result.push_str(identifier);
            result.push(',');
        }
        result.pop();// Remove last comma
        result.push_str("){\n");
        for statement in &self.statements {
            result.push_str(format!(
                "    {}\n",
                statement.transpile(program, &self.environment)).as_str());
        }
        result.push_str("}\n");
        result
    }

    pub fn data_type(&self) -> Option<DataType> {
        self.return_type.clone()
    }

    pub fn call(&mut self) -> Option<Constant> {
        None
    }
}

#[allow(dead_code)]
pub struct FunctionCall {
    // Functions vector index
    index: usize,
    dtype: Option<DataType>,
    arguments: Vec<Box<Evaluable>>,
}

#[allow(dead_code)]
impl FunctionCall {
    pub fn new(index: usize, dtype: Option<DataType>, arguments: Vec<Box<Evaluable>>) -> FunctionCall {
        FunctionCall { index, dtype, arguments }
    }

    pub fn get_index(&self) -> usize {
        return self.index;
    }
}

impl Evaluable for FunctionCall {
    fn data_type(&self) -> Option<DataType> {
        self.dtype.clone()
    }

    fn eval(&self, program: &mut Program) -> Option<Constant> {
        program.call_function(self.index)
    }
}