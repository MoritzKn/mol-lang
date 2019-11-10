use crate::ast;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, Weak};
use std::{fmt, fmt::Display};

type ScopeChain = Vec<Arc<Mutex<Scope>>>;
type WeakScopeChain = Vec<Weak<Mutex<Scope>>>;

#[derive(Debug, Clone)]
pub struct Closure {
    name: String,
    scope_chain: ScopeChain,
    slots: Vec<ast::Slot>,
    expression: ast::Expression,
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Propper formating
        write!(f, "[Function: {}]", self.name)
    }
}

type NativeFunction = fn(Vec<Value>) -> Result<Value, Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Function(Box<Closure>),
    NativeFunction(NativeFunction),
}

impl Value {
    pub fn get_type(&self) -> String {
        match self {
            Value::Void => String::from("Void"),
            Value::Number(_) => String::from("Number"),
            Value::String(_) => String::from("String"),
            Value::List(_) => String::from("List"),
            Value::Map(_) => String::from("Map"),
            Value::Function(_) => String::from("Function"),
            Value::NativeFunction(_) => String::from("NativeFunction"),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Void => "void".to_owned(),
            Value::Number(value) => value.to_string(),
            Value::String(value) => value.clone(),
            Value::List(value) => value
                .iter()
                .map(|v| v.as_string())
                .collect::<Vec<String>>()
                .join(", "),
            Value::Map(value) => format!("{:?}", value),
            Value::Function(value) => format!("{}", value),
            Value::NativeFunction(value) => format!("NativeFunction({:?})", value),
        }
    }

    pub fn print(&self) -> String {
        match self {
            Value::String(value) => format!("\"{}\"", value),
            Value::List(value) => format!(
                "[{}]",
                value
                    .iter()
                    .map(|v| v.print())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            _ => self.as_string(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "{}", self.get_type()),
            Value::NativeFunction(_) => write!(f, "{}", self.as_string()),
            _ => write!(f, "{}({})", self.get_type(), self.as_string()),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Void
    }
}

impl From<f64> for Value {
    fn from(number: f64) -> Value {
        Value::Number(number)
    }
}

impl From<String> for Value {
    fn from(string: String) -> Value {
        Value::String(string)
    }
}

impl From<&str> for Value {
    fn from(string: &str) -> Value {
        Value::String(string.to_owned())
    }
}

impl From<Vec<Value>> for Value {
    fn from(list: Vec<Value>) -> Value {
        Value::List(list)
    }
}

impl From<HashMap<String, Value>> for Value {
    fn from(map: HashMap<String, Value>) -> Value {
        Value::Map(map)
    }
}

impl From<Closure> for Value {
    fn from(func: Closure) -> Value {
        Value::Function(Box::new(func))
    }
}

impl From<NativeFunction> for Value {
    fn from(func: NativeFunction) -> Value {
        Value::NativeFunction(func)
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

#[derive(Debug, Clone)]
pub struct Context {
    current: Arc<Mutex<Frame>>,
    stack: Vec<Arc<Mutex<Frame>>>,
}

impl Context {
    pub fn new() -> Self {
        let mut scope = Frame::new();

        scope.set_var(
            "log".to_owned(),
            Value::NativeFunction(|args| {
                let text = args
                    .iter()
                    .map(|v| v.as_string())
                    .collect::<Vec<String>>()
                    .join(" ");

                println!("{}", text);

                Ok(Value::Void)
            }),
        );

        scope.set_var(
            "dummyList".to_owned(),
            Value::from(vec![Value::from("foo"), Value::from("bar")]),
        );

        scope.set_var(
            "typeof".to_owned(),
            Value::NativeFunction(|args| {
                args.get(0)
                    .map(|v| Value::from(v.get_type()))
                    .ok_or_else(|| Value::from("TypeError: Expected one argument but got 0"))
            }),
        );

        let mut math = HashMap::new();

        math.insert(
            "sum".to_owned(),
            Value::NativeFunction(|args| {
                args.iter()
                    .try_fold(0.0, |acc, curr| match curr {
                        Value::Number(curr) => Ok(acc + curr),
                        _ => Err(Value::from(format!(
                            "TypeError: Can not sum elements of type {}",
                            curr.get_type()
                        ))),
                    })
                    .map(Value::Number)
            }),
        );

        scope.set_var("Math".to_owned(), Value::Map(math));

        Self {
            current: Arc::new(Mutex::new(scope)),
            stack: vec![],
        }
    }

    fn get_var(&self, id: &ast::Id) -> Option<Value> {
        self.current.lock().unwrap().get_var(&id.name)
    }

    fn set_var(&mut self, id: &ast::Id, value: Value) {
        self.current.lock().unwrap().set_var(id.name.clone(), value)
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

        let mut chain = current
            .scope_chain
            .iter()
            .map(|scope| scope.upgrade().expect("Scope already dropped"))
            .collect::<ScopeChain>();
        chain.push(current.scope.clone());

        chain
    }
}
fn eval_func(closure: Closure, mut args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let frame = Frame::for_closure(closure.scope_chain);
    let frame = Arc::new(Mutex::new(frame));

    ctx.push_stack(frame);

    for (i, arg) in args.drain(..).enumerate() {
        ctx.set_var(&closure.slots[i].id, arg);
    }

    let result = eval_expr(closure.expression, ctx);

    ctx.pop_stack();

    result
}

fn call_fn(call: ast::Call, ctx: &mut Context) -> Result<Value, Value> {
    let callee = eval_expr(call.callee, ctx)?;

    let mut args = vec![];
    for expr in call.arguments {
        let value = eval_expr(expr, ctx)?;
        args.push(value);
    }

    match callee {
        Value::Function(func) => eval_func(*func, args, ctx),
        Value::NativeFunction(func) => func(args),
        _ => Err(Value::from(format!(
            "TypeError: {} is not a function",
            callee.print()
        ))),
    }
}

fn eval_expr_list(expr_list: Vec<ast::Expression>, ctx: &mut Context) -> Result<Value, Value> {
    let mut final_val = Value::Void;
    for expr in expr_list {
        final_val = eval_expr(expr, ctx)?;
    }
    Ok(final_val)
}

fn eval_expr(expr: ast::Expression, ctx: &mut Context) -> Result<Value, Value> {
    use ast::Expression::*;

    match expr {
        Call(call) => call_fn(*call, ctx),
        MemberAccess(member_access) => {
            let object = eval_expr(member_access.object, ctx)?;

            if let Value::Map(map) = object {
                Ok(map
                    .get(&member_access.property.name)
                    .cloned()
                    .unwrap_or_default())
            } else {
                Err(Value::from(format!(
                    "TypeError: Can not access property '{}' of {}",
                    member_access.property,
                    object.print()
                )))
            }
        }
        Declaration(declaration) => {
            let value = eval_expr(declaration.value, ctx)?;
            ctx.set_var(&declaration.id, value.clone());
            Ok(value)
        }
        Block(block) => eval_expr_list(block.body, ctx),
        Id(id) => ctx
            .get_var(&id)
            .ok_or_else(|| Value::from(format!("ReferenceError: {} is not defined", id))),
        NumberLiteral(number_literal) => Ok(Value::from(number_literal.value)),
        StringLiteral(string_literal) => Ok(Value::from(string_literal.value)),
        Function(function) => {
            let value = Value::from(Closure {
                name: function.id.to_owned().name,
                expression: function.expression,
                slots: function.slots,
                scope_chain: ctx.export_scope_chain(),
            });
            ctx.set_var(&function.id, value.clone());
            Ok(value)
        }
        Lambda(function) => Ok(Value::from(Closure {
            name: "anonymous".to_owned(),
            expression: function.expression,
            slots: function.slots,
            scope_chain: ctx.export_scope_chain(),
        })),
    }
}

pub fn exec_with_context(program: ast::Program, ctx: &mut Context) -> Result<Value, Value> {
    eval_expr_list(program.body, ctx)
}

pub fn exec(program: ast::Program) -> Result<Value, Value> {
    let mut context = Context::new();
    eval_expr_list(program.body, &mut context)
}
