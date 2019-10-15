use std::{fmt, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub body: Vec<Expression>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        join(f, &self.body, ";\n")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Block(Box<Block>),
    Call(Box<Call>),
    MemberAccess(Box<MemberAccess>),
    Declaration(Box<Declaration>),
    Id(Box<Id>),
    FunctionLiteral(Box<FunctionLiteral>),
    NumberLiteral(Box<NumberLiteral>),
    StringLiteral(Box<StringLiteral>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Block(value) => write!(f, "{}", value),
            Expression::Call(value) => write!(f, "{}", value),
            Expression::MemberAccess(value) => write!(f, "{}", value),
            Expression::Declaration(value) => write!(f, "{}", value),
            Expression::Id(value) => write!(f, "{}", value),
            Expression::FunctionLiteral(value) => write!(f, "{}", value),
            Expression::NumberLiteral(value) => write!(f, "{}", value),
            Expression::StringLiteral(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub body: Vec<Expression>,
}

impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;

        let mut body = String::new();
        join(&mut body, &self.body, ";\n")?;
        let body = body
            .split('\n')
            .map(|line| format!("    {}", line))
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{}", body)?;

        write!(f, "\n}}")
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
        join(f, &self.arguments, " ,")?;
        write!(f, ")")
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
pub struct FunctionLiteral {
    pub id: Id,
    pub slots: Vec<Slot>,
    pub expression: Expression,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        join(f, &self.slots, " ,")?;
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

fn join<W: fmt::Write, T: Display>(f: &mut W, list: &[T], seperator: &str) -> fmt::Result {
    let mut first = true;
    for item in list {
        if !first {
            write!(f, "{}", seperator)?;
        };
        write!(f, "{}", item)?;
        first = false;
    }
    Ok(())
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
    pub fn expr_function_literal(function_literal: FunctionLiteral) -> Expression {
        Expression::FunctionLiteral(Box::new(function_literal))
    }
    pub fn expr_string_literal(string_literal: StringLiteral) -> Expression {
        Expression::StringLiteral(Box::new(string_literal))
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

    pub fn functio_literal(id: Id, slots: Vec<Slot>, expression: Expression) -> FunctionLiteral {
        FunctionLiteral {
            id,
            slots,
            expression,
        }
    }

    pub fn functio_literal_expr(id: Id, slots: Vec<Slot>, expression: Expression) -> Expression {
        expr_function_literal(functio_literal(id, slots, expression))
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
}
