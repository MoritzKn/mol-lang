use crate::ast::{Expression, Identifier, Program};

enum ExpressionTail {
    MemberAccess(Identifier),
    Call(Vec<Expression>),
}

mod grammar {
    #![allow(clippy::all)]
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
        let ast = program(vec![call_stm(identifier_expr("test"), vec![])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_call_with_args() {
        let result = parse_string(r#"test(42, "hello")"#).unwrap();
        let ast = program(vec![call_stm(
            identifier_expr("test"),
            vec![number_literal_expr(42f64), string_literal_expr("hello")],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_call_spaces() {
        let result = parse_string(r#" test ( 42 , "hello" ) ; "#).unwrap();
        let ast = program(vec![call_stm(
            identifier_expr("test"),
            vec![number_literal_expr(42f64), string_literal_expr("hello")],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_trailing_comma() {
        let result = parse_string(r#"test(42,)"#).unwrap();
        let ast = program(vec![call_stm(
            identifier_expr("test"),
            vec![number_literal_expr(42f64)],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_member_access() {
        let result = parse_string(r#"test.foo()"#).unwrap();
        let ast = program(vec![call_stm(
            member_access_expr(identifier_expr("test"), identifier("foo")),
            vec![],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration() {
        let result = parse_string(r#"let foo = 42"#).unwrap();
        let ast = program(vec![declaration_stm(
            identifier("foo"),
            number_literal_expr(42.0),
        )]);

        assert_eq!(result, ast);
    }
}
