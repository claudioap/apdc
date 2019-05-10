use crate::yggl::expression::Expression;
use crate::yggl::statement::Statement;
use crate::yggl::environment::Environment;

#[allow(dead_code)]
pub struct Conditional {
    condition: Option<Expression>,
    statements: Vec<Statement>,
    sibling: Option<Box<Conditional>>,
}

#[allow(dead_code)]
impl Conditional {
    pub fn new(condition: Option<Expression>, statements: Vec<Statement>) -> Conditional {
        Conditional {
            condition: condition,
            statements,
            sibling: None,
        }
    }

    pub fn add_sibling(&mut self, condition: Option<Expression>, statements: Vec<Statement>) {
        if let Some(sibling) = &mut self.sibling {
            sibling.add_sibling(condition, statements);
        } else {
            self.sibling = Some(Box::new(Conditional::new(condition, statements)));
        }
    }

    pub fn transpile(&self, env: &Environment) -> String {
        let mut output = String::new();
        if let Some(condition) = &self.condition {
            output.push_str(format!("if ({}) {{\n", condition.transpile(env)).as_str());
        } else {
            output.push_str("{\n");
        }
        for statement in &self.statements {
            output.push_str(format!("    {}\n", statement.transpile(env)).as_str());
        }
        if let Some(sibling) = &self.sibling {
            output.push_str("} else ");
            output.push_str(sibling.transpile(env).as_str());
        } else {
            output.push_str("}");
        }
        return output;
    }
}