#[cfg(test)]
mod tests {
    use pest::Parser;
    use crate::yggl::expression::Expression;
    use crate::yggl::data::{Constant, DataType};
    use crate::yggl::environment::Environment;
    use crate::parser::{YGGLParser, Rule};

    #[test]
    fn expression_precedence_climber() {
        let program = "1+10*100";
        if let Ok(mut pairs) = YGGLParser::parse(Rule::expression, program) {
            let pair = pairs.next().unwrap();
            if pair.as_rule() != Rule::expression {
                panic!("Parsed the wrong kind of data");
            }
            let env = Environment::new();
            if let Ok(expression) = Expression::from(pair, &env) {
                assert_eq!(expression.data_type().unwrap(), DataType::Int);
                assert_eq!(expression.eval(&Environment::new()),
                           Constant::Int(1001));
            } else {
                panic!("Unable to parse expression");
            }
        } else {
            panic!("Unable to parse expression");
        }
    }
}