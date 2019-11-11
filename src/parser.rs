use crate::ast::{Binary, BinaryOperator, Call, Expression, Id, MemberAccess, Program};

enum ExpressionTail {
    MemberAccess(Id),
    Call(Vec<Expression>),
    Binary(BinaryOperator, Expression),
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
            ExpressionTail::Binary(op, right) => Expression::Binary(Box::new(Binary {
                left: expr,
                op,
                right,
            })),
        }
    }
}

mod grammar {
    #![allow(warnings, clippy::all)]
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

pub type ParseError = grammar::ParseError;

pub fn parse_string(string: &str) -> Result<Program, ParseError> {
    grammar::program(string)
}

#[cfg(test)]
mod tests {
    use super::parse_string;
    use crate::ast::build::*;

    #[test]
    fn text_id() {
        let result = parse_string(r#"foo"#).unwrap();
        let ast = program(vec![id_expr("foo")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_id_access() {
        let result = parse_string(r#"test.foo()"#).unwrap();
        let ast = program(vec![call_expr(
            member_access_expr(id_expr("test"), id("foo")),
            vec![],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_multiple_exprs() {
        let result = parse_string(r#"foo;bar;"#).unwrap();
        let ast = program(vec![id_expr("foo"), id_expr("bar")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration() {
        let result = parse_string(r#"let foo = 42"#).unwrap();
        let ast = program(vec![declaration_expr(id("foo"), number_literal_expr(42.0))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration_with_member_access() {
        let result = parse_string(r#"let foo = bar.baz"#).unwrap();
        let ast = program(vec![declaration_expr(
            id("foo"),
            member_access_expr(id_expr("bar"), id("baz")),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration_member_access() {
        let result = parse_string(r#"(let foo = 42).baz"#).unwrap();
        let ast = program(vec![member_access_expr(
            declaration_expr(id("foo"), number_literal_expr(42.0)),
            id("baz"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_declaration_no_space_after_keyword() {
        let result = parse_string(r#"letfoo = 42"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_function() {
        let result = parse_string(r#"function foo () {}"#).unwrap();
        let ast = program(vec![function_expr(id("foo"), vec![], block_expr(vec![]))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_member_access() {
        let result = parse_string(r#"function foo () {}.foo"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_function_no_space_after_keyword() {
        let result = parse_string(r#"functionfoo () {}"#);
        assert!(result.is_err());
    }

    #[test]
    fn test_function_with_args() {
        let result = parse_string(r#"function foo (a, b) { a; b }"#).unwrap();
        let ast = program(vec![function_expr(
            id("foo"),
            vec![slot("a", "Any"), slot("b", "Any")],
            block_expr(vec![id_expr("a"), id_expr("b")]),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_with_args_and_types() {
        let result = parse_string(r#"function foo (a: B) { a }"#).unwrap();
        let ast = program(vec![function_expr(
            id("foo"),
            vec![slot("a", "B")],
            block_expr(vec![id_expr("a")]),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_lambda() {
        let result = parse_string(r#"() => {}"#).unwrap();
        let ast = program(vec![lambda_expr(vec![], block_expr(vec![]))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_lambda_with_member_access() {
        let result = parse_string(r#"() => {}.foo"#).unwrap();
        let ast = program(vec![lambda_expr(
            vec![],
            member_access_expr(block_expr(vec![]), id("foo")),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_lambda_with_args() {
        let result = parse_string(r#"(a, b) => { a; b }"#).unwrap();
        let ast = program(vec![lambda_expr(
            vec![slot("a", "Any"), slot("b", "Any")],
            block_expr(vec![id_expr("a"), id_expr("b")]),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_lambda_with_args_and_types() {
        let result = parse_string(r#"(a: B) => { a }"#).unwrap();
        let ast = program(vec![lambda_expr(
            vec![slot("a", "B")],
            block_expr(vec![id_expr("a")]),
        )]);

        assert_eq!(result, ast);
    }

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
    fn test_function_call_trailing_comma() {
        let result = parse_string(r#"test(42,)"#).unwrap();
        let ast = program(vec![call_expr(
            id_expr("test"),
            vec![number_literal_expr(42.0)],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_call_expr_in_args() {
        let result = parse_string(r#"add(40 + 1, 1)"#).unwrap();
        let ast = program(vec![call_expr(
            id_expr("add"),
            vec![
                add_expr(number_literal_expr(40.0), number_literal_expr(1.0)),
                number_literal_expr(1.0),
            ],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_block() {
        let result = parse_string(r#"{ foo; bar }"#).unwrap();
        let ast = program(vec![block_expr(vec![id_expr("foo"), id_expr("bar")])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_block_member_access() {
        let result = parse_string(r#"{ foo; bar }.foo"#).unwrap();
        let ast = program(vec![member_access_expr(
            block_expr(vec![id_expr("foo"), id_expr("bar")]),
            id("foo"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_literal() {
        let result = parse_string(r#"42"#).unwrap();
        let ast = program(vec![number_literal_expr(42.0)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_literal_member_access() {
        let result = parse_string(r#"42..bar"#).unwrap();
        let ast = program(vec![member_access_expr(
            number_literal_expr(42.0),
            id("bar"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_literal_decimal() {
        let result = parse_string(r#"42.24"#).unwrap();
        let ast = program(vec![number_literal_expr(42.24)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_literal_decimal_member_access() {
        let result = parse_string(r#"42.24.bar"#).unwrap();
        let ast = program(vec![member_access_expr(
            number_literal_expr(42.24),
            id("bar"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_string_literal() {
        let result = parse_string(r#" "foo" "#).unwrap();
        let ast = program(vec![string_literal_expr("foo")]);

        assert_eq!(result, ast);
    }

    // TODO: Fix this
    // #[test]
    // fn test_string_literal_escape() {
    //     let result = parse_string(r#" "foo: \\" "#).unwrap();
    //     let ast = program(vec![string_literal_expr("foo: \\")]);
    //
    //     assert_eq!(result, ast);
    // }

    #[test]
    fn test_string_literal_member_access() {
        let result = parse_string(r#" "foo".bar "#).unwrap();
        let ast = program(vec![member_access_expr(
            string_literal_expr("foo"),
            id("bar"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_boolean_literal_true() {
        let result = parse_string(r#"true"#).unwrap();
        let ast = program(vec![boolean_literal_expr(true)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_boolean_literal_false() {
        let result = parse_string(r#"false"#).unwrap();
        let ast = program(vec![boolean_literal_expr(false)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_boolean_literal_and_id() {
        let result = parse_string(r#"falsefoo"#).unwrap();
        let ast = program(vec![id_expr("falsefoo")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_void_literal() {
        let result = parse_string(r#"void"#).unwrap();
        let ast = program(vec![void_literal_expr()]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_void_literal_and_id() {
        let result = parse_string(r#"voidfoo"#).unwrap();
        let ast = program(vec![id_expr("voidfoo")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_parentheses() {
        let result = parse_string(r#" (foo(("bar"))).bar "#).unwrap();
        let ast = program(vec![member_access_expr(
            call_expr(id_expr("foo"), vec![string_literal_expr("bar")]),
            id("bar"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_not() {
        let result = parse_string(r#"!false"#).unwrap();
        let ast = program(vec![not_expr(boolean_literal_expr(false))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_positive() {
        let result = parse_string(r#"+false"#).unwrap();
        let ast = program(vec![pos_expr(boolean_literal_expr(false))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_negative() {
        let result = parse_string(r#"-false"#).unwrap();
        let ast = program(vec![neg_expr(boolean_literal_expr(false))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_multiple_unary() {
        let result = parse_string(r#"! - + false"#).unwrap();
        let ast = program(vec![not_expr(neg_expr(pos_expr(boolean_literal_expr(
            false,
        ))))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_add_and_unary() {
        let result = parse_string(r#"1 + - + 1"#).unwrap();
        let ast = program(vec![add_expr(
            number_literal_expr(1.0),
            neg_expr(pos_expr(number_literal_expr(1.0))),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_concat() {
        let result = parse_string(r#"1 ++ 1"#).unwrap();
        let ast = program(vec![concat_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_add() {
        let result = parse_string(r#"1 + 1"#).unwrap();
        let ast = program(vec![add_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_sub() {
        let result = parse_string(r#"1 - 1"#).unwrap();
        let ast = program(vec![sub_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_mul() {
        let result = parse_string(r#"1 * 1"#).unwrap();
        let ast = program(vec![mul_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_div() {
        let result = parse_string(r#"1 / 1"#).unwrap();
        let ast = program(vec![div_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_or() {
        let result = parse_string(r#"1 or 1"#).unwrap();
        let ast = program(vec![or_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_and() {
        let result = parse_string(r#"1 and 1"#).unwrap();
        let ast = program(vec![and_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_eq() {
        let result = parse_string(r#"1 == 1"#).unwrap();
        let ast = program(vec![eq_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_ne() {
        let result = parse_string(r#"1 != 1"#).unwrap();
        let ast = program(vec![ne_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_ge() {
        let result = parse_string(r#"1 >= 1"#).unwrap();
        let ast = program(vec![ge_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_le() {
        let result = parse_string(r#"1 <= 1"#).unwrap();
        let ast = program(vec![le_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_gt() {
        let result = parse_string(r#"1 > 1"#).unwrap();
        let ast = program(vec![gt_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_lt() {
        let result = parse_string(r#"1 < 1"#).unwrap();
        let ast = program(vec![lt_expr(
            number_literal_expr(1.0),
            number_literal_expr(1.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_multiline_comment() {
        let result = parse_string(r#"/* test */ 42"#).unwrap();
        let ast = program(vec![number_literal_expr(42.0)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_line_comment() {
        let result = parse_string("// test \n 42").unwrap();
        let ast = program(vec![number_literal_expr(42.0)]);

        assert_eq!(result, ast);
    }
}
