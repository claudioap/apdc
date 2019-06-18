use std::rc::Rc;
use std::cell::RefCell;
use crate::yggl::environment::{Environment, Variable};
use crate::yggl::statement::Statement;
use crate::yggl::data::{DataType, Constant};
use crate::yggl::annotation;
use crate::parser::CompilationError;
use crate::yggl::expression::Expression;

/// The concept of a function, which is treated like subprogram within the program.
/// Functions have their own environments. Outside variables aren't accessible from the inside,
/// BUT outside static variables should be accessible (TODO)
pub struct Function {
    name: String,
    parameters: Vec<Rc<Variable>>,
    environment: Environment,
    statements: Vec<Statement>,
    return_type: RefCell<Option<DataType>>,
    exported: bool,
}

#[allow(dead_code)]
impl Function {
    pub fn new(environment: Environment, name: String, parameters: Vec<Rc<Variable>>,
               mut statements: Vec<Statement>) -> Result<Function, CompilationError> {
        annotation::expand_statements(&mut statements);
        annotation::propagate_types(&statements);
        annotation::insert_declarations(&mut statements);
        let function = Function {
            name,
            parameters,
            environment,
            statements,
            return_type: RefCell::new(None),
            exported: false,
        };
        function.determine_return()?;
        Ok(function)
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn get_return(&self) -> Option<DataType> {
        self.return_type.borrow().clone()
    }

    pub fn is_exported(&self) -> bool {
        self.exported
    }

    pub fn set_exported(&mut self) {
        self.exported = true;
    }

    fn determine_return(&self) -> Result<(), CompilationError> {
        let mut dtype = self.return_type.borrow().clone();
        for statement in &self.statements {
            if let Statement::Return(ref evaluable) = statement {
                if dtype == None {
                    dtype = evaluable.data_type();
                } else if dtype != evaluable.data_type() {
                    return Err(
                        CompilationError::new(
                            0, 0, "".to_string(),
                            format!(
                                "Function {} returns two different data types ({} and {})",
                                self.name,
                                dtype.unwrap().transpile(),
                                evaluable.data_type().unwrap().transpile())));
                }
            }
        }
        self.return_type.replace(dtype);
        Ok(())
    }

    pub fn set_parameter_type(&self, index: usize, dtype: DataType) -> Result<(), CompilationError> {
        match self.parameters.get(index) {
            Some(var) => var.set_type(dtype),
            None => {
                return Err(CompilationError::new(
                    0, 0, "".to_string(),
                    format!("Function {} has no parameter in index {}", self.name, index)));
            }
        }
        self.determine_return()
    }

    pub fn transpile_signature(&self) -> String {
        let rtype = match &self.return_type.borrow().clone() {
            Some(dtype) => dtype.transpile(),
            _ => "void".to_string()
        };
        let mut result = String::new();
        result.reserve(1024 * 10); // Reserve 10 KB to prevent further allocations
        result.push_str(format!("{} {}(", rtype, self.name).as_str());
        for parameter in &self.parameters {
            let identifier = parameter.get_identifier();
            let dtype = parameter.data_type()
                .expect(format!(
                    "Parameter {} has unknown type ({})",
                    identifier, self.name).as_str());
            result.push_str(dtype.transpile().as_str());
            result.push(' ');
            result.push_str(identifier);
            result.push(',');
        }
        if !self.parameters.is_empty() {
            result.pop();// Remove last comma
        }
        result.push_str(");");
        result
    }

    pub fn transpile(&self) -> String {
        let rtype = match &self.return_type.borrow().clone() {
            Some(dtype) => dtype.transpile(),
            _ => "void".to_string()
        };
        let mut result = String::new();
        result.reserve(1024 * 10); // Reserve 10 KB to prevent further allocations
        result.push_str(format!("{} {}(", rtype, self.name).as_str());
        for parameter in &self.parameters {
            let identifier = parameter.get_identifier();
            let dtype = parameter.data_type()
                .expect(format!(
                    "Parameter {} has unknown type ({})",
                    identifier, self.name).as_str());
            result.push_str(dtype.transpile().as_str());
            result.push(' ');
            result.push_str(identifier);
            result.push(',');
        }
        if self.parameters.is_empty() {
            result.push_str("){\n");
        } else {
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
        self.return_type.borrow().clone()
    }

    pub fn call(&self) -> Option<Constant> {
        None
    }

    pub fn env_dump(&self) {
        self.environment.print_snapshot();
    }
}

#[allow(dead_code)]
#[derive(Clone)]
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

    pub fn data_type(&self) -> Option<DataType> {
        self.function.data_type()
    }

    fn eval(&self) -> Option<Constant> {
        self.function.call()
    }

    pub fn transpile(&self) -> String {
        let mut output = format!("{}(", self.function.get_name());
        for argument in &self.arguments {
            output.push_str(format!("{},", argument.transpile()).as_str())
        }
        output.pop();
        output.push(')');
        output
    }
}