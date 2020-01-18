use crate::ast;
use std::collections::HashMap;

#[derive(Default, Debug, Clone)]
struct Context {
    var_name_map: HashMap<String, String>,
    depth: usize,

    // is_stm
}

fn gen_expr(expr: &ast::Expression, ctx: &mut Context) -> String {
    ctx.depth += 1;
    let result = gen_expr_inner(expr, ctx);
    ctx.depth -= 1;

    result
}

fn gen_expr_inner(expr: &ast::Expression, ctx: &mut Context) -> String {
    use ast::Expression::*;

    match expr {
        Binary(binary) => {
            let left = gen_expr(&binary.left, ctx);
            let right = gen_expr(&binary.right, ctx);

            use ast::BinaryOperator::*;
            match binary.op {
                Add => format!("({} + {})", left, right),
                Sub => format!("({} - {})", left, right),
                Mul => format!("({} * {})", left, right),
                Div => format!("({} / {})", left, right),
                Or => format!("({} || {})", left, right),
                And => format!("({} && {})", left, right),
                Eq => format!("({} === {})", left, right),
                Ne => format!("({} !== {})", left, right),
                Gt => format!("({} > {})", left, right),
                Lt => format!("({} < {})", left, right),
                Ge => format!("({} >= {})", left, right),
                Le => format!("({} <= {})", left, right),
                Concat => format!("('' + {} + {})", left, right),
            }
        }
        Block(block) => {
            // FIXME: This is wrong JS syntax
            format!(
                "{{ {} }}",
                block
                    .body
                    .iter()
                    .map(|expr| gen_expr(expr, ctx))
                    .collect::<Vec<_>>()
                    .join("; ")
            )
        }
        IfElse(if_else) => {
            // FIXME: This is wrong JS syntax
            format!("{}", if_else)
        }
        BooleanLiteral(boolean_literal) => {
            // FIXME: This is wrong JS syntax
            format!("{}", boolean_literal)
        }
        Call(call) => {
            // FIXME: This is wrong JS syntax
            format!("{}", call)
        }
        Declaration(declaration) => {
            // FIXME: This is wrong JS syntax
            format!(
                "let {} = {};",
                declaration.id,
                gen_expr(&declaration.value, ctx)
            )
        }
        Function(function) => {
            // FIXME: This is wrong JS syntax
            format!(
                "function {}({}) {}",
                function.id,
                function
                    .slots
                    .iter()
                    .map(|slot| slot.id.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                gen_expr(&function.expression, ctx),
            )
        }
        Id(id) => {
            // FIXME: This is wrong JS syntax
            format!("{}", id)
        }
        Lambda(lambda) => {
            // FIXME: This is wrong JS syntax
            format!(
                "(({}) => {})",
                lambda
                    .slots
                    .iter()
                    .map(|slot| slot.id.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                gen_expr(&lambda.expression, ctx),
            )
        }
        MemberAccess(member_access) => {
            // FIXME: This is wrong JS syntax
            format!(
                "({}).{}",
                gen_expr(&member_access.object, ctx),
                member_access.property
            )
        }
        NumberLiteral(number_literal) => {
            // FIXME: This is wrong JS syntax
            format!("{}", number_literal)
        }
        StringLiteral(string_literal) => {
            // FIXME: This is wrong JS syntax
            format!("{}", string_literal)
        }
        Unary(unary) => {
            // FIXME: This is wrong JS syntax
            let value = gen_expr(&unary.expr, ctx);

            use ast::UnaryOperator::*;
            match unary.op {
                Not => format!("!{}", value),
                Neg => format!("-{}", value),
                Pos => format!("+{}", value),
            }
        }
        VoidLiteral(_) => {
            // FIXME: This is wrong JS syntax
            format!("null")
        }
    }
}

pub fn generate_js(program: &ast::Program) -> String {
    let mut ctx = Context::default();

    program
        .body
        .iter()
        .map(|expr| gen_expr(expr, &mut ctx))
        .collect::<Vec<_>>()
        .join(";")
}
