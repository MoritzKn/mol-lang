use crate::ast::*;
use std::collections::HashMap;
use std::{fmt, fmt::Display};

// #[derive(Debug, Clone, PartialEq)]
// struct Instance {
//     name: String,
//     value: HashMap(String, Value),
// }

// TODO: Introduce void type
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    // Map(HashMap<String, Value>),
    // List(Vec<Value>),
    // Struct(HashMap<String, Value>, Box<Struct>),
    Function(Box<FunctionLiteral>),
    NativeFunction(fn(Vec<Value>) -> Result<Value, Throw>),
}

impl Value {
    fn get_type(&self) -> String {
        match self {
            Value::Void => String::from("Void"),
            Value::Number(_) => String::from("Number"),
            Value::String(_) => String::from("String"),
            Value::Function(_) => String::from("Function"),
            Value::NativeFunction(_) => String::from("NativeFunction"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Number(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Function(_) => write!(f, "Function"),
            Value::NativeFunction(_) => write!(f, "NativeFunction"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    stack: Vec<HashMap<String, Value>>,
}

impl Context {
    pub fn new() -> Self {
        let mut scope = HashMap::new();
        scope.insert(
            "log".to_string(),
            Value::NativeFunction(|args| {
                let args: Vec<String> = args.iter().map(|v| format!("{:?}", v)).collect();
                println!("{}", args.join(""));

                Ok(Value::Void)
            }),
        );

        scope.insert(
            "add".to_string(),
            Value::NativeFunction(|args| {
                let mut sum = 0.0;

                for arg in args {
                    if let Value::Number(value) = arg {
                        sum += value;
                    };
                }

                Ok(Value::Number(sum))
            }),
        );

        Self { stack: vec![scope] }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        self.stack.last().unwrap().get(name).cloned()
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
pub struct Throw {
    pub value: Value,
}

fn throw(value: Value) -> Result<Value, Throw> {
    Err(Throw { value })
}

fn eval_func(func: FunctionLiteral, args: Vec<Value>, ctx: &mut Context) -> Result<Value, Throw> {
    // set args in ctx
    eval_expr(func.expression, ctx)
}

fn call_fn(call: Call, ctx: &mut Context) -> Result<Value, Throw> {
    let callee = eval_expr(call.callee, ctx)?;

    let mut args = vec![];
    for expr in call.arguments {
        let value = eval_expr(expr, ctx)?;
        args.push(value);
    }

    match callee {
        Value::Function(func) => eval_func(*func, args, ctx),
        Value::NativeFunction(func) => func(args),
        _ => throw(Value::String(format!(
            "TypeError: {} is not a function",
            callee
        ))),
    }
}

fn eval_expr(expr: Expression, ctx: &mut Context) -> Result<Value, Throw> {
    match expr {
        Expression::Call(call) => call_fn(*call, ctx),
        Expression::MemberAccess(member_access) => {
            let object = eval_expr(member_access.object, ctx)?;
            // object.get(property.name);
            Ok(object)
        }
        Expression::Declaration(declaration) => {
            let value = eval_expr(declaration.value, ctx)?;
            ctx.set_var(declaration.id.name, value.clone());
            Ok(value)
        }
        Expression::Block(block) => {
            let mut final_val = Value::Void;
            for expression in block.body {
                final_val = eval_expr(expression, ctx)?;
            }
            Ok(final_val)
        }
        Expression::Identifier(identifier) => ctx.get_var(&identifier.name).ok_or(Throw {
            value: Value::String(format!("ReferenceError: {} not defined", &identifier.name)),
        }),
        Expression::FunctionLiteral(functio_literal) => Ok(Value::Function(functio_literal)),
        Expression::NumberLiteral(number_literal) => Ok(Value::Number(number_literal.value)),
        Expression::StringLiteral(string_literal) => Ok(Value::String(string_literal.value)),
    }
}

pub fn exec_with_context(program: Program, ctx: &mut Context) -> Result<Value, Throw> {
    eval_expr(program.content, ctx)
}
