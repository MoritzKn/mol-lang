use crate::ast::*;
use std::collections::HashMap;
use std::{fmt, fmt::Display};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub struct Closure {
    function: FunctionLiteral,
    scope: Arc<Mutex<Scope>>,
}

// #[derive(Debug, Clone, PartialEq)]
// struct Instance {
//     name: String,
//     value: HashMap(String, Value),
// }

// TODO: Introduce void type
#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    // Map(HashMap<String, Value>),
    // List(Vec<Value>),
    // Struct(HashMap<String, Value>, Box<Struct>),
    Function(Box<Closure>),
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

    fn to_string(&self) -> String {
        match self {
            Value::Void => "void".to_string(),
            Value::Number(value) => value.to_string(),
            Value::String(value) => value.clone(),
            Value::Function(value) => format!("Function({:?})", value),
            Value::NativeFunction(value) => format!("NativeFunction({:?})", value),
        }
    }

    fn print(&self) -> String {
        match self {
            Value::String(value) => format!("\"{}\"", value),
            _ => self.to_string(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Function(_) | Value::NativeFunction(_) => write!(f, "{}", self.to_string()),
            _ => write!(f, "{}({})", self.get_type(), self.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    chain: Vec<Arc<Mutex<HashMap<String, Value>>>>,
}

impl Scope {
    fn new() -> Self {
        Scope { chain: vec![Arc::new(Mutex::new(HashMap::new()))] }
    }

    fn extending(scope: Arc<Mutex<Scope>>) -> Self {
        let scope = scope.lock().unwrap();
        let mut chain = scope.chain.clone();
        chain.push(Arc::new(Mutex::new(HashMap::new())));

        Scope { chain }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for map in self.chain.iter().rev() {
            let map = map.lock().unwrap();
            let value = map.get(name);
            if value.is_some() {
                return value.map(|value| value.clone());
            }
        }
        return None;
    }

    fn set_var(&mut self, name: &str, value: Value) {
        let mut last = self.chain.last().unwrap().lock().unwrap();
        last.insert(name.to_string(), value);
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    current_scope: Arc<Mutex<Scope>>,
    stack: Vec<Arc<Mutex<Scope>>>,
}

impl Context {
    pub fn new() -> Self {
        let mut scope = Scope::new();

        scope.set_var("log",
            Value::NativeFunction(|args| {
                let args: Vec<String> = args.iter().map(|v| v.to_string()).collect();
                println!("{}", args.join(" "));

                Ok(Value::Void)
            }),
        );

        scope.set_var("add",
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

        Self { current_scope: Arc::new(Mutex::new(scope)), stack: vec![] }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        let scope = self.current_scope.lock().unwrap();
        scope.get_var(name)
    }

    fn set_var(&mut self, name: &str, value: Value) {
        let mut scope = self.current_scope.lock().unwrap();
        scope.set_var(name, value)
    }

    fn stack_push(&mut self, scope: Arc<Mutex<Scope>>) {
        self.stack.push(self.current_scope.clone());
        self.current_scope = scope;
    }

    fn stack_pop(&mut self) {
        if let Some(scope) = self.stack.pop() {
            self.current_scope = scope;
        } else {
            panic!("Can not pop empty stack");
        }
    }
}

#[derive(Debug, Clone)]
pub struct Throw {
    pub value: Value,
}

fn throw(value: Value) -> Result<Value, Throw> {
    Err(Throw { value })
}

fn eval_func(closure: Closure, args: Vec<Value>, ctx: &mut Context) -> Result<Value, Throw> {
    let scope = Scope::extending(closure.scope);

    ctx.stack_push(Arc::new(Mutex::new(scope)));

    let result = eval_expr(closure.function.expression, ctx);

    ctx.stack_pop();

    result
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
            callee.print()
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
            ctx.set_var(&declaration.id.name, value.clone());
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
        Expression::FunctionLiteral(functio_literal) => {
            Ok(Value::Function(Box::new(Closure{
                function: *functio_literal,
                scope: ctx.current_scope.clone(),
            })))
        },
        Expression::NumberLiteral(number_literal) => Ok(Value::Number(number_literal.value)),
        Expression::StringLiteral(string_literal) => Ok(Value::String(string_literal.value)),
    }
}

pub fn exec_with_context(program: Program, ctx: &mut Context) -> Result<Value, Throw> {
    eval_expr(program.content, ctx)
}
