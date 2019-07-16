use crate::ast::*;
use std::collections::HashMap;

// #[derive(Debug, Clone, PartialEq)]
// struct Instance {
//     name: String,
//     value: HashMap(String, Value),
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    NativeFunction(fn(Vec<Value>) -> Option<Value>),
}

#[derive(Debug, Clone, PartialEq)]
struct Context {
    stack: Vec<HashMap<String, Value>>,
}

impl Context {
    fn new() -> Self {
        let mut scope = HashMap::new();
        scope.insert(
            "log".to_string(),
            Value::NativeFunction(|args| {
                let args: Vec<String> = args.iter().map(|v| format!("{:?}", v)).collect();
                println!("{}", args.join(""));

                None
            }),
        );

        Self { stack: vec![scope] }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        self.stack.last().unwrap().get(name).map(|v| v.clone())
    }

    fn set_var(&mut self, name: String, value: Value) {
        self.stack.last_mut().unwrap().insert(name, value);
    }

    fn stack_push(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn stack_pop(&mut self) {
        self.stack.pop();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeError {}

// fn eval_func(function: Function, args: Vec<Value>, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
//
// }

fn call_fn(call: Call, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
    let func = eval_expr(call.callee, ctx)?;

    if let Some(func) = func {
        let mut args = vec![];
        for expr in call.arguments {
            let arg = eval_expr(expr, ctx)?;

            if arg.is_none() {
                return Err(RuntimeError {})
            }

            args.push(arg.unwrap());
        }

        match func {
            Value::NativeFunction(func) => Ok(func(args)),
            _ => Err(RuntimeError {}),
        }
    } else {
        Err(RuntimeError {})
    }
}

fn eval_expr(expr: Expression, ctx: &mut Context) -> Result<Option<Value>, RuntimeError> {
    match expr {
        Expression::Call(call) => call_fn(*call, ctx),
        Expression::MemberAccess(member_access) => {
            let object = eval_expr(member_access.object, ctx)?;
            // object.get(property.name);
            Ok(object)
        }
        Expression::Declaration(declaration) => {
            let value = eval_expr(declaration.value, ctx)?;
            if let Some(value) = value {
                ctx.set_var(declaration.id.name, value.clone());
                Ok(Some(value))
            } else {
                Err(RuntimeError{})
            }
        }
        Expression::Identifier(identifier) => Ok(ctx.get_var(&identifier.name)),
        Expression::NumberLiteral(number_literal) => Ok(Some(Value::Number(number_literal.value))),
        Expression::StringLiteral(string_literal) => Ok(Some(Value::String(string_literal.value))),
    }
}

pub fn exec(program: Program) -> Result<Option<Value>, RuntimeError> {
    let mut ctx = Context::new();

    let mut final_val = None;
    for statement in program.body {
        let result = match statement {
            Statement::Expression(expression) => eval_expr(expression, &mut ctx),
        };

        if result.is_err() {
            return result;
        }

        if let Ok(value) = result {
            final_val = value
        }
    }

    Ok(final_val)
}
