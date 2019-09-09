use crate::ast;
use std::collections::HashMap;
use std::sync::{Weak, Arc, Mutex};
use std::{fmt, fmt::Display};

type ScopeChain = Vec<Arc<Mutex<Scope>>>;
type WeakScopeChain = Vec<Weak<Mutex<Scope>>>;

#[derive(Debug, Clone)]
pub struct Closure {
    function: ast::FunctionLiteral,
    scope_chain: ScopeChain,
}

// #[derive(Debug, Clone, PartialEq)]
// struct Instance {
//     name: String,
//     value: HashMap(String, Value),
// }

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
    pub fn get_type(&self) -> String {
        match self {
            Value::Void => String::from("Void"),
            Value::Number(_) => String::from("Number"),
            Value::String(_) => String::from("String"),
            Value::Function(_) => String::from("Function"),
            Value::NativeFunction(_) => String::from("NativeFunction"),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Value::Void => "void".to_string(),
            Value::Number(value) => value.to_string(),
            Value::String(value) => value.clone(),
            Value::Function(value) => format!("{:?}", value),
            Value::NativeFunction(value) => format!("NativeFunction({:?})", value),
        }
    }

    pub fn print(&self) -> String {
        match self {
            Value::String(value) => format!("\"{}\"", value),
            _ => self.to_string(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "{}", self.get_type()),
            Value::NativeFunction(_) => write!(f, "{}", self.to_string()),
            _ => write!(f, "{}({})", self.get_type(), self.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    vars: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        self.vars.get(name).cloned()
    }

    fn set(&mut self, name: String, value: Value) {
        self.vars.insert(name, value);
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    scope: Arc<Mutex<Scope>>,
    scope_chain: WeakScopeChain,
}

impl Frame {
    fn new() -> Self {
        Self {
            scope: Arc::new(Mutex::new(Scope::new())),
            scope_chain: vec![],
        }
    }

    fn for_closure(scope_chain: ScopeChain) -> Self {
        Self {
            scope: Arc::new(Mutex::new(Scope::new())),
            scope_chain: scope_chain
                .iter()
                .map(|scope| Arc::downgrade(scope))
                .collect::<WeakScopeChain>(),
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        let scope = self.scope.lock().unwrap();
        let value = scope.get(name);
        if value.is_some() {
            return value;
        }

        for scope in self.scope_chain.iter().rev() {
            let scope = scope.upgrade().expect("Scope already dropped");
            let scope = scope.lock().unwrap();
            let value = scope.get(name);
            if value.is_some() {
                return value;
            }
        }

        None
    }

    fn set_var(&mut self, name: String, value: Value) {
        let mut scope = self.scope.lock().unwrap();
        scope.set(name, value);
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "scope({})", self.scope_chain.len())
    }
}

impl Drop for Frame {
    fn drop(&mut self) {
        println!("[!] Dropping {}", self);
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    current: Arc<Mutex<Frame>>,
    stack: Vec<Arc<Mutex<Frame>>>,
}

impl Context {
    pub fn new() -> Self {
        let mut scope = Frame::new();

        scope.set_var(
            "log".to_string(),
            Value::NativeFunction(|args| {
                let args: Vec<String> = args.iter().map(|v| v.to_string()).collect();
                println!("{}", args.join(" "));

                Ok(Value::Void)
            }),
        );

        scope.set_var(
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

        Self {
            current: Arc::new(Mutex::new(scope)),
            stack: vec![],
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        self.current.lock().unwrap().get_var(name)
    }

    fn set_var(&mut self, name: String, value: Value) {
        self.current.lock().unwrap().set_var(name, value)
    }

    fn push_stack(&mut self, frame: Arc<Mutex<Frame>>) {
        self.stack.push(self.current.clone());
        self.current = frame;
    }

    fn pop_stack(&mut self) {
        if let Some(frame) = self.stack.pop() {
            self.current = frame;
        } else {
            panic!("Can not pop empty stack");
        }
    }

    fn export_scope_chain(&self) -> ScopeChain {
        let current = self.current.lock().unwrap();

        let mut chain = current.scope_chain
            .iter()
            .map(|scope| scope.upgrade().expect("Scope already dropped"))
            .collect::<ScopeChain>();
        chain.push(current.scope.clone());

        chain
    }
}

#[derive(Debug, Clone)]
pub struct Throw {
    pub value: Value,
}

fn throw(value: Value) -> Result<Value, Throw> {
    Err(Throw { value })
}

fn eval_func(closure: Closure, mut args: Vec<Value>, ctx: &mut Context) -> Result<Value, Throw> {
    let frame = Frame::for_closure(closure.scope_chain);
    let frame = Arc::new(Mutex::new(frame));

    ctx.push_stack(frame);

    for (i, arg) in args.drain(..).enumerate() {
        ctx.set_var(closure.function.slots[i].id.name.clone(), arg);
    }

    let result = eval_expr(closure.function.expression, ctx);

    ctx.pop_stack();

    result
}

fn call_fn(call: ast::Call, ctx: &mut Context) -> Result<Value, Throw> {
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

fn eval_expr(expr: ast::Expression, ctx: &mut Context) -> Result<Value, Throw> {
    use ast::Expression::*;

    match expr {
        Call(call) => call_fn(*call, ctx),
        MemberAccess(member_access) => {
            let object = eval_expr(member_access.object, ctx)?;
            // object.get(property.name);
            Ok(object)
        }
        Declaration(declaration) => {
            let value = eval_expr(declaration.value, ctx)?;
            ctx.set_var(declaration.id.name, value.clone());
            Ok(value)
        }
        Block(block) => {
            let mut final_val = Value::Void;
            for expression in block.body {
                final_val = eval_expr(expression, ctx)?;
            }
            Ok(final_val)
        }
        Identifier(identifier) => ctx.get_var(&identifier.name).ok_or(Throw {
            value: Value::String(format!("ReferenceError: {} not defined", &identifier.name)),
        }),
        FunctionLiteral(functio_literal) => Ok(Value::Function(Box::new(Closure {
            function: *functio_literal,
            scope_chain: ctx.export_scope_chain(),
        }))),
        NumberLiteral(number_literal) => Ok(Value::Number(number_literal.value)),
        StringLiteral(string_literal) => Ok(Value::String(string_literal.value)),
    }
}

pub fn exec_with_context(program: ast::Program, ctx: &mut Context) -> Result<Value, Throw> {
    eval_expr(program.content, ctx)
}
