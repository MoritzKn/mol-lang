use crate::utils::write_list;
use std::{fmt, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Expression>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_list(f, &self.body, ";\n")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Binary(Box<Binary>),
    Block(Box<Block>),
    BooleanLiteral(Box<BooleanLiteral>),
    Call(Box<Call>),
    Declaration(Box<Declaration>),
    Function(Box<Function>),
    Id(Box<Id>),
    Lambda(Box<Lambda>),
    MemberAccess(Box<MemberAccess>),
    NumberLiteral(Box<NumberLiteral>),
    StringLiteral(Box<StringLiteral>),
    Unary(Box<Unary>),
    VoidLiteral(Box<VoidLiteral>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Binary(ast) => write!(f, "{}", ast),
            Expression::Block(ast) => write!(f, "{}", ast),
            Expression::BooleanLiteral(ast) => write!(f, "{}", ast),
            Expression::Call(ast) => write!(f, "{}", ast),
            Expression::Declaration(ast) => write!(f, "{}", ast),
            Expression::Function(ast) => write!(f, "{}", ast),
            Expression::Id(ast) => write!(f, "{}", ast),
            Expression::Lambda(ast) => write!(f, "{}", ast),
            Expression::MemberAccess(ast) => write!(f, "{}", ast),
            Expression::NumberLiteral(ast) => write!(f, "{}", ast),
            Expression::StringLiteral(ast) => write!(f, "{}", ast),
            Expression::Unary(ast) => write!(f, "{}", ast),
            Expression::VoidLiteral(ast) => write!(f, "{}", ast),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub body: Vec<Expression>,
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let len = self.body.len();

        if len == 0 {
            writeln!(f, "{{}}")
        } else if len == 1 {
            writeln!(f, "{{ {} }}", self.body.get(0).unwrap())
        } else {
            writeln!(f, "{{")?;

            let mut body = String::new();
            write_list(&mut body, &self.body, ";\n")?;
            let body = body
                .split('\n')
                .map(|line| format!("    {}", line))
                .collect::<Vec<String>>()
                .join("\n");
            write!(f, "{}", body)?;

            write!(f, "\n}}")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expression,
    pub arguments: Vec<Expression>,
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.callee)?;
        write_list(f, &self.arguments, " ,")?;
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Or,
    And,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Concat,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Sub => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::Ne => write!(f, "!="),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::Ge => write!(f, ">="),
            BinaryOperator::Le => write!(f, "<="),
            BinaryOperator::Concat => write!(f, "++"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Expression,
    pub op: BinaryOperator,
    pub right: Expression,
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.op, self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Not,
    Neg,
    Pos,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::Pos => write!(f, "+"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub op: UnaryOperator,
    pub expr: Expression,
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.op, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    pub object: Expression,
    pub property: Id,
}

impl Display for MemberAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.object, self.property)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub id: Id,
    pub value: Expression,
}

impl Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.id, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Id {
    pub name: String,
}

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Slot {
    pub id: Id,
    pub ty: Id,
}

impl Display for Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub id: Id,
    pub slots: Vec<Slot>,
    pub expression: Expression,
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function {}(", self.id.name)?;
        write_list(f, &self.slots, ", ")?;
        write!(f, ") {}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub slots: Vec<Slot>,
    pub expression: Expression,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        write_list(f, &self.slots, " ,")?;
        write!(f, ") => {}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
}

impl Display for NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VoidLiteral {}

impl Display for VoidLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "void")
    }
}

#[allow(dead_code)]
pub mod build {
    use super::*;

    pub fn program(body: Vec<Expression>) -> Program {
        Program { body }
    }

    pub fn expr_block(block: Block) -> Expression {
        Expression::Block(Box::new(block))
    }
    pub fn expr_call(call: Call) -> Expression {
        Expression::Call(Box::new(call))
    }
    pub fn expr_member_access(member_access: MemberAccess) -> Expression {
        Expression::MemberAccess(Box::new(member_access))
    }
    pub fn expr_declaration(declaration: Declaration) -> Expression {
        Expression::Declaration(Box::new(declaration))
    }
    pub fn expr_id(id: Id) -> Expression {
        Expression::Id(Box::new(id))
    }
    pub fn expr_number_literal(number_literal: NumberLiteral) -> Expression {
        Expression::NumberLiteral(Box::new(number_literal))
    }
    pub fn expr_string_literal(string_literal: StringLiteral) -> Expression {
        Expression::StringLiteral(Box::new(string_literal))
    }
    pub fn expr_boolean_literal(boolean_literal: BooleanLiteral) -> Expression {
        Expression::BooleanLiteral(Box::new(boolean_literal))
    }
    pub fn expr_void_literal(void_literal: VoidLiteral) -> Expression {
        Expression::VoidLiteral(Box::new(void_literal))
    }
    pub fn expr_function(function: Function) -> Expression {
        Expression::Function(Box::new(function))
    }
    pub fn expr_lambda(lambda: Lambda) -> Expression {
        Expression::Lambda(Box::new(lambda))
    }
    pub fn expr_binary(binary: Binary) -> Expression {
        Expression::Binary(Box::new(binary))
    }
    pub fn expr_unary(uary: Unary) -> Expression {
        Expression::Unary(Box::new(uary))
    }

    pub fn binop_add() -> BinaryOperator {
        BinaryOperator::Add
    }
    pub fn binop_sub() -> BinaryOperator {
        BinaryOperator::Sub
    }
    pub fn binop_mul() -> BinaryOperator {
        BinaryOperator::Mul
    }
    pub fn binop_div() -> BinaryOperator {
        BinaryOperator::Div
    }
    pub fn binop_or() -> BinaryOperator {
        BinaryOperator::Or
    }
    pub fn binop_and() -> BinaryOperator {
        BinaryOperator::And
    }
    pub fn binop_eq() -> BinaryOperator {
        BinaryOperator::Eq
    }
    pub fn binop_ne() -> BinaryOperator {
        BinaryOperator::Ne
    }
    pub fn binop_gt() -> BinaryOperator {
        BinaryOperator::Gt
    }
    pub fn binop_lt() -> BinaryOperator {
        BinaryOperator::Lt
    }
    pub fn binop_ge() -> BinaryOperator {
        BinaryOperator::Ge
    }
    pub fn binop_le() -> BinaryOperator {
        BinaryOperator::Le
    }
    pub fn binop_concat() -> BinaryOperator {
        BinaryOperator::Concat
    }

    pub fn add_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_add(), right)
    }
    pub fn sub_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_sub(), right)
    }
    pub fn mul_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_mul(), right)
    }
    pub fn div_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_div(), right)
    }
    pub fn or_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_or(), right)
    }
    pub fn and_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_and(), right)
    }
    pub fn eq_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_eq(), right)
    }
    pub fn ne_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_ne(), right)
    }
    pub fn gt_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_gt(), right)
    }
    pub fn lt_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_lt(), right)
    }
    pub fn ge_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_ge(), right)
    }
    pub fn le_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_le(), right)
    }
    pub fn concat_expr(left: Expression, right: Expression) -> Expression {
        binary_expr(left, binop_concat(), right)
    }

    pub fn unop_not() -> UnaryOperator {
        UnaryOperator::Not
    }
    pub fn unop_neg() -> UnaryOperator {
        UnaryOperator::Neg
    }
    pub fn unop_pos() -> UnaryOperator {
        UnaryOperator::Pos
    }

    pub fn not_expr(expr: Expression) -> Expression {
        unary_expr(expr, unop_not())
    }
    pub fn neg_expr(expr: Expression) -> Expression {
        unary_expr(expr, unop_neg())
    }
    pub fn pos_expr(expr: Expression) -> Expression {
        unary_expr(expr, unop_pos())
    }

    pub fn block(body: Vec<Expression>) -> Block {
        Block { body }
    }

    pub fn block_expr(body: Vec<Expression>) -> Expression {
        Expression::Block(Box::new(block(body)))
    }

    pub fn call(callee: Expression, arguments: Vec<Expression>) -> Call {
        Call { callee, arguments }
    }

    pub fn call_expr(callee: Expression, arguments: Vec<Expression>) -> Expression {
        expr_call(call(callee, arguments))
    }

    pub fn member_access(object: Expression, property: Id) -> MemberAccess {
        MemberAccess { object, property }
    }

    pub fn member_access_expr(object: Expression, property: Id) -> Expression {
        expr_member_access(member_access(object, property))
    }

    pub fn declaration(id: Id, value: Expression) -> Declaration {
        Declaration { id, value }
    }
    pub fn declaration_expr(id: Id, value: Expression) -> Expression {
        expr_declaration(declaration(id, value))
    }

    pub fn id(name: &str) -> Id {
        Id {
            name: name.to_owned(),
        }
    }
    pub fn id_expr(name: &str) -> Expression {
        expr_id(id(name))
    }

    pub fn slot(name: &str, ty: &str) -> Slot {
        Slot {
            id: id(name),
            ty: id(ty),
        }
    }

    pub fn number_literal(value: f64) -> NumberLiteral {
        NumberLiteral { value }
    }
    pub fn number_literal_expr(value: f64) -> Expression {
        expr_number_literal(number_literal(value))
    }

    pub fn string_literal(value: &str) -> StringLiteral {
        StringLiteral {
            value: value.to_owned(),
        }
    }
    pub fn string_literal_expr(value: &str) -> Expression {
        expr_string_literal(string_literal(value))
    }

    pub fn boolean_literal(value: bool) -> BooleanLiteral {
        BooleanLiteral { value }
    }
    pub fn boolean_literal_expr(value: bool) -> Expression {
        expr_boolean_literal(boolean_literal(value))
    }

    pub fn void_literal() -> VoidLiteral {
        VoidLiteral {}
    }
    pub fn void_literal_expr() -> Expression {
        expr_void_literal(void_literal())
    }

    pub fn function(id: Id, slots: Vec<Slot>, expression: Expression) -> Function {
        Function {
            id,
            slots,
            expression,
        }
    }
    pub fn function_expr(id: Id, slots: Vec<Slot>, expression: Expression) -> Expression {
        expr_function(function(id, slots, expression))
    }

    pub fn lambda(slots: Vec<Slot>, expression: Expression) -> Lambda {
        Lambda { slots, expression }
    }
    pub fn lambda_expr(slots: Vec<Slot>, expression: Expression) -> Expression {
        expr_lambda(lambda(slots, expression))
    }

    pub fn binary(left: Expression, op: BinaryOperator, right: Expression) -> Binary {
        Binary { left, op, right }
    }
    pub fn binary_expr(left: Expression, op: BinaryOperator, right: Expression) -> Expression {
        expr_binary(binary(left, op, right))
    }

    pub fn unary(expr: Expression, op: UnaryOperator) -> Unary {
        Unary { expr, op }
    }
    pub fn unary_expr(expr: Expression, op: UnaryOperator) -> Expression {
        expr_unary(unary(expr, op))
    }
}
