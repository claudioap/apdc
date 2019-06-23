use crate::yggl::expression::Expression;
use crate::yggl::statement::Statement;
use crate::yggl::environment::Environment;
use crate::yggl::data::Constant;

pub struct Conditional {
    condition: Option<Expression>,
    statements: Vec<Statement>,
    sibling: Option<Box<Conditional>>,
}

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
            output.push_str(format!("if ({}) {{\n", condition.transpile()).as_str());
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

pub enum Cycle {
    Loop(Vec<Box<Statement>>),
    While(Expression, Vec<Box<Statement>>),
    DoWhile(Expression, Vec<Box<Statement>>),
    For(Box<Statement>, Expression, Option<Box<Statement>>, Vec<Box<Statement>>),
}

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
                output.push_str(format!("while ({}) {{\n", condition.transpile()).as_str());
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
                output.push_str(format!("}} while ({});", condition.transpile()).as_str());
            }
            Cycle::For(initialization, condition, posrun, statements) => {
                if let Some(posrun_statement) = posrun {
                    output.push_str(
                        format!(
                            "for({};{};{}){{\n",
                            initialization.transpile(env),
                            condition.transpile(),
                            posrun_statement.transpile(env)
                        ).as_str());
                } else {
                    output.push_str(
                        format!(
                            "for({};{};){{\n",
                            initialization.transpile(env),
                            condition.transpile()
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

pub struct Switch {
    exp: Expression,
    cases: Vec<Case>
}

impl Switch {
    pub fn new(exp: Expression, cases: Vec<Case>) -> Switch {
        Switch{exp, cases}
    }
    pub fn transpile(&self, env: &Environment) -> String{
        let mut output = format!("switch({}){{\n", self.exp.transpile());
        for case in &*self.cases {
            let mut case_transp = case.transpile(env);
            case_transp = case_transp.replace("\n", "\n\t");
            output.push('\t');
            output.push_str(case_transp.as_str());
            output.push('\n');
        }
        output.push_str("};");
        output
    }
}

pub struct Case {
    value: Constant,
    statements: Vec<Statement>
}

impl Case {
    pub fn new(value: Constant, statements: Vec<Statement>) -> Case {
        Case{value, statements}
    }

    pub fn transpile(&self, env: &Environment) -> String{
        let mut output = format!("case {}:\n", self.value);
        for statement in &*self.statements{
            output.push_str(format!("\t{}\n", statement.transpile(env)).as_str());
        }
        output.push_str("\tbreak;");
        output
    }
}