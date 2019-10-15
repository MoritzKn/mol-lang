use crate::ast::{Call, Expression, Id, MemberAccess, Program};

enum ExpressionTail {
    MemberAccess(Id),
    Call(Vec<Expression>),
}

impl ExpressionTail {
    // NOTE: Usage in grammar not recognised
    #![allow(dead_code)]
    pub fn prepend(self, expr: Expression) -> Expression {
        match self {
            ExpressionTail::MemberAccess(property) => {
                Expression::MemberAccess(Box::new(MemberAccess {
                    object: expr,
                    property,
                }))
            }
            ExpressionTail::Call(arguments) => Expression::Call(Box::new(Call {
                callee: expr,
                arguments,
            })),
        }
    }
}

mod grammar {
    #![allow(warnings, clippy::all)]
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

pub fn parse_string(string: &str) -> Result<Program, grammar::ParseError> {
    grammar::program(string)
}

#[cfg(test)]
mod tests {
    use super::parse_string;
    use crate::ast::build::*;

    #[test]
    fn test_function_call() {
        let result = parse_string(r#"test()"#).unwrap();
        let ast = program(vec![call_expr(id_expr("test"), vec![])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_call_with_args() {
        let result = parse_string(r#"test(42, "hello")"#).unwrap();
        let ast = program(vec![call_expr(
            id_expr("test"),
            vec![number_literal_expr(42.0), string_literal_expr("hello")],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_call_spaces() {
        let result = parse_string(r#" test ( 42 , "hello" ) ; "#).unwrap();
        let ast = program(vec![call_expr(
            id_expr("test"),
            vec![number_literal_expr(42.0), string_literal_expr("hello")],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_trailing_comma() {
        let result = parse_string(r#"test(42,)"#).unwrap();
        let ast = program(vec![call_expr(
            id_expr("test"),
            vec![number_literal_expr(42.0)],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_member_access() {
        let result = parse_string(r#"test.foo()"#).unwrap();
        let ast = program(vec![call_expr(
            member_access_expr(id_expr("test"), id("foo")),
            vec![],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration() {
        let result = parse_string(r#"let foo = 42"#).unwrap();
        let ast = program(vec![declaration_expr(id("foo"), number_literal_expr(42.0))]);

        assert_eq!(result, ast);
    }
}
