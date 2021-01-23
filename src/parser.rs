use crate::ast::{Bind, Call, Expression, Id, MemberAccess, Program};

enum ExpressionTail {
    MemberAccess(Id),
    Bind(Expression),
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
            ExpressionTail::Bind(method) => Expression::Bind(Box::new(Bind {
                object: expr,
                method,
            })),
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

pub type ParseError = grammar::ParseError;

pub fn parse_string(string: &str) -> Result<Program, ParseError> {
    grammar::program(string)
}

#[cfg(test)]
mod tests {
    use super::parse_string;
    use crate::ast::build::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_id() {
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
    fn test_declaration_invalid_identifier() {
        let result = parse_string(r#"let let = bar.baz"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_declaration_no_space_after_keyword() {
        let result = parse_string(r#"letfoo = bar.baz"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_function() {
        let result = parse_string(r#"function foo () {}"#).unwrap();
        let ast = program(vec![function_expr(id("foo"), vec![], block_expr(vec![]))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_expr_member_access() {
        let result = parse_string(r#"function foo () {}.foo"#).unwrap();
        let ast = program(vec![member_access_expr(
            function_expr(id("foo"), vec![], block_expr(vec![])),
            id("foo"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_expr_call() {
        let result = parse_string(r#"function foo () {}()"#).unwrap();
        let ast = program(vec![call_expr(
            function_expr(id("foo"), vec![], block_expr(vec![])),
            vec![],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_function_invalid_identifier() {
        let result = parse_string(r#"function function () {}"#);

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
    fn test_lambda_with_call() {
        let result = parse_string(r#"() => {}()"#).unwrap();
        let ast = program(vec![lambda_expr(
            vec![],
            call_expr(block_expr(vec![]), vec![]),
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
    fn test_function_call_invalid_identifier() {
        let result = parse_string(r#"let()"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_bind() {
        let result = parse_string(r#"foo:bar"#).unwrap();
        let ast = program(vec![bind_expr(id_expr("foo"), id_expr("bar"))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_bind_call() {
        let result = parse_string(r#"foo:bar()"#).unwrap();
        let ast = program(vec![call_expr(
            bind_expr(id_expr("foo"), id_expr("bar")),
            vec![],
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_bind_member_access() {
        let result = parse_string(r#"foo:bar.baz"#).unwrap();
        let ast = program(vec![bind_expr(
            id_expr("foo"),
            member_access_expr(id_expr("bar"), id("baz")),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_dobble_bind_call() {
        let result = parse_string(r#"foo:bar:baz()"#).unwrap();
        let ast = program(vec![call_expr(
            bind_expr(bind_expr(id_expr("foo"), id_expr("bar")), id_expr("baz")),
            vec![],
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
    fn test_if_member_access() {
        let result = parse_string(r#"if (foo) {bar}.foo"#).unwrap();
        let ast = program(vec![if_expr(
            id_expr("foo"),
            member_access_expr(block_expr(vec![id_expr("bar")]), id("foo")),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_if_block() {
        let result = parse_string(r#"if (foo) {bar}"#).unwrap();
        let ast = program(vec![if_expr(
            id_expr("foo"),
            block_expr(vec![id_expr("bar")]),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_if_else() {
        let result = parse_string(r#"if (foo) bar else baz"#).unwrap();
        let ast = program(vec![if_else_expr(
            id_expr("foo"),
            id_expr("bar"),
            id_expr("baz"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_if_else_block() {
        let result = parse_string(r#"if (foo) {bar} else {baz}"#).unwrap();
        let ast = program(vec![if_else_expr(
            id_expr("foo"),
            block_expr(vec![id_expr("bar")]),
            block_expr(vec![id_expr("baz")]),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_if_else_block_no_space() {
        let result = parse_string(r#"if (foo) {bar} elsebaz"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_number_literal() {
        let result = parse_string(r#"42"#).unwrap();
        let ast = program(vec![number_literal_expr(42.0)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_nan() {
        let result = parse_string(r#"NaN"#).unwrap();
        let ast = program(vec![number_literal_expr(std::f64::NAN)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_infinity() {
        let result = parse_string(r#"Infinity"#).unwrap();
        let ast = program(vec![number_literal_expr(std::f64::INFINITY)]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_number_neg_infinity() {
        let result = parse_string(r#"-Infinity"#).unwrap();
        let ast = program(vec![neg_expr(number_literal_expr(std::f64::INFINITY))]);

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
    fn test_number_literal_negative_member_access() {
        let result = parse_string(r#"-42..bar"#).unwrap();
        let ast = program(vec![neg_expr(member_access_expr(
            number_literal_expr(42.0),
            id("bar"),
        ))]);

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
    fn test_string_literal_double_quote() {
        let result = parse_string(r#" "foo 'bar' " "#).unwrap();
        let ast = program(vec![string_literal_expr("foo 'bar' ")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_string_literal_single_quote() {
        let result = parse_string(r#" 'foo "bar" ' "#).unwrap();
        let ast = program(vec![string_literal_expr("foo \"bar\" ")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_string_literal_escape_quote() {
        let result = parse_string(r#" "foo: \\" "#).unwrap();
        let ast = program(vec![string_literal_expr("foo: \\")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_string_literal_escape_backspace() {
        let result = parse_string(r#" "foo\\bar" "#).unwrap();
        let ast = program(vec![string_literal_expr("foo\\bar")]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_string_literal_escape_nl() {
        let result = parse_string(r#" "foo\nbar" "#).unwrap();
        let ast = program(vec![string_literal_expr("foo\nbar")]);

        assert_eq!(result, ast);
    }

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
    fn test_not_member_access() {
        let result = parse_string(r#"!foo.bar"#).unwrap();
        let ast = program(vec![not_expr(member_access_expr(
            id_expr("foo"),
            id("bar"),
        ))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_not_precedence() {
        let result = parse_string(r#"!foo and bar"#).unwrap();
        let ast = program(vec![and_expr(not_expr(id_expr("foo")), id_expr("bar"))]);

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
    fn test_negative_member() {
        let result = parse_string(r#"-foo.bar"#).unwrap();
        let ast = program(vec![neg_expr(member_access_expr(
            id_expr("foo"),
            id("bar"),
        ))]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_negative_precedence() {
        let result = parse_string(r#"-1 * 2"#).unwrap();
        let ast = program(vec![mul_expr(
            neg_expr(number_literal_expr(1.0)),
            number_literal_expr(2.0),
        )]);

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
    fn test_or_no_space() {
        let result = parse_string(r#"bar orfoo"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_or_no_space_before() {
        let result = parse_string(r#"baror foo"#);

        assert!(result.is_err());
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
    fn test_and_no_space() {
        let result = parse_string(r#"bar andfoo"#);

        assert!(result.is_err());
    }

    #[test]
    fn test_and_no_space_before() {
        let result = parse_string(r#"barand foo"#);

        assert!(result.is_err());
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
    fn test_operator_precedence_increasing() {
        let result = parse_string(r#"1 + 2 * 3"#).unwrap();
        let ast = program(vec![add_expr(
            number_literal_expr(1.0),
            mul_expr(number_literal_expr(2.0), number_literal_expr(3.0)),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_operator_precedence_decreasing() {
        let result = parse_string(r#"1 * 2 + 3"#).unwrap();
        let ast = program(vec![add_expr(
            mul_expr(number_literal_expr(1.0), number_literal_expr(2.0)),
            number_literal_expr(3.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_operator_precedence_constant() {
        let result = parse_string(r#"1 + 2 + 3"#).unwrap();
        let ast = program(vec![add_expr(
            add_expr(number_literal_expr(1.0), number_literal_expr(2.0)),
            number_literal_expr(3.0),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_operator_precedence_parentheses() {
        let result = parse_string(r#"(1 + 2) * 3"#).unwrap();
        let ast = program(vec![mul_expr(
            add_expr(number_literal_expr(1.0), number_literal_expr(2.0)),
            number_literal_expr(3.0),
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

    #[test]
    fn test_conditional_and_or() {
        let result =
            parse_string("num <= 1 and 1 or fibonacci(num - 1) + fibonacci(num - 2)").unwrap();
        let ast = program(vec![or_expr(
            and_expr(
                le_expr(id_expr("num"), number_literal_expr(1.0)),
                number_literal_expr(1.0),
            ),
            add_expr(
                call_expr(
                    id_expr("fibonacci"),
                    vec![sub_expr(id_expr("num"), number_literal_expr(1.0))],
                ),
                call_expr(
                    id_expr("fibonacci"),
                    vec![sub_expr(id_expr("num"), number_literal_expr(2.0))],
                ),
            ),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_conditional_and_or_simple() {
        let result = parse_string("a and b or c").unwrap();
        let ast = program(vec![or_expr(
            and_expr(id_expr("a"), id_expr("b")),
            id_expr("c"),
        )]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_list_literal() {
        let result = parse_string("[1, 2, 3]").unwrap();
        let ast = program(vec![list_literal_expr(vec![
            number_literal_expr(1.0),
            number_literal_expr(2.0),
            number_literal_expr(3.0),
        ])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_list_literal_nested() {
        let result = parse_string("[1, [2, 3]]").unwrap();
        let ast = program(vec![list_literal_expr(vec![
            number_literal_expr(1.0),
            list_literal_expr(vec![number_literal_expr(2.0), number_literal_expr(3.0)]),
        ])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_list_literal_string() {
        let result = parse_string("['foo [] bar']").unwrap();
        let ast = program(vec![list_literal_expr(vec![string_literal_expr(
            "foo [] bar",
        )])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_multiple_statements() {
        let result = parse_string("test();test()").unwrap();
        let ast = program(vec![
            call_expr(id_expr("test"), vec![]),
            call_expr(id_expr("test"), vec![]),
        ]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_trailing_semicolon() {
        let result = parse_string("test();").unwrap();
        let ast = program(vec![call_expr(id_expr("test"), vec![])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_leading_semicolon() {
        let result = parse_string(";test()").unwrap();
        let ast = program(vec![call_expr(id_expr("test"), vec![])]);

        assert_eq!(result, ast);
    }

    #[test]
    fn test_multiple_semicolons() {
        let result = parse_string(";;;").unwrap();
        let ast = program(vec![]);

        assert_eq!(result, ast);
    }
}
