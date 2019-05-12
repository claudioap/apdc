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
            condition,
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

#[allow(dead_code)]
pub enum Cycle {
    Loop(Vec<Box<Statement>>),
    While(Expression, Vec<Box<Statement>>),
    DoWhile(Expression, Vec<Box<Statement>>),
    For(Box<Statement>, Expression, Option<Box<Statement>>, Vec<Box<Statement>>),
}

#[allow(dead_code)]
impl Cycle {
    pub fn transpile(&self, env: &Environment) -> String {
        let mut output = String::new();
        match self {
            Cycle::Loop(statements) => {
                output.push_str("while (1 == 1) {\n");
                for statement in statements {
                    output.push_str(format!("    {}\n", statement.transpile(env)).as_str());
                }
                output.push('}');
            }
            Cycle::While(condition, statements) => {
                output.push_str(format!("while {} {{\n", condition.transpile(env)).as_str());
                for statement in statements {
                    output.push_str(format!("    {}\n", statement.transpile(env)).as_str());
                }
                output.push('}');
            }
            Cycle::DoWhile(condition, statements) => {
                output.push_str("do {\n");
                for statement in statements {
                    output.push_str(format!("    {}\n", statement.transpile(env)).as_str());
                }
                output.push_str(format!("}} while {};", condition.transpile(env)).as_str());
            }
            Cycle::For(initialization, condition, posrun, statements) => {
                if let Some(posrun_statement) = posrun {
                    output.push_str(
                        format!(
                            "for({};{};{}){{\n",
                            initialization.transpile(env),
                            condition.transpile(env),
                            posrun_statement.transpile(env)
                        ).as_str());
                } else {
                    output.push_str(
                        format!(
                            "for({};{};){{\n",
                            initialization.transpile(env),
                            condition.transpile(env)
                        ).as_str());
                }
                for statement in statements {
                    output.push_str(format!("    {}\n", statement.transpile(env)).as_str());
                }
                output.push('}');
            }
        }
        output
    }
}
