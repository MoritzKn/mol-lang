use crate::ast;
use crate::utils::write_list;
use std::collections::HashMap;
use std::sync::{Arc, Mutex, Weak};
use std::{fmt, fmt::Display};

type ScopeRef = Arc<Mutex<Scope>>;
type ScopeRefWeak = Weak<Mutex<Scope>>;

#[cfg(debug_assertions)]
fn inspect_scope_chain(chain: &[ScopeRef]) {
    let mut i = 0;
    for scope in chain {
        i += 1;
        print!("layer{}: ", i);
        let scope = scope.lock().unwrap();
        scope.print();
    }
}

#[cfg(debug_assertions)]
pub fn inspect_scope_chain_binding(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    let arg = args
        .into_iter()
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 1 argument but got 0"))?;

    match arg {
        Value::Function(func) => match func {
            Function::Closure(closue) => Ok(Value::from(inspect_scope_chain(&closue.scope_chain))),
            _ => Err(Value::from(
                "TypeError: Only closure functions have a scope chain",
            )),
        },
        _ => Err(Value::from(format!(
            "TypeError: Expected function but got {}",
            arg.get_type()
        ))),
    }
}

#[cfg(debug_assertions)]
fn inspect_scope(scope: &ScopeRef) {
    let scope = scope.lock().unwrap();
    scope.print();
}

#[cfg(debug_assertions)]
pub fn inspect_scope_binding(_args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    Ok(Value::from(inspect_scope(
        &ctx.current.lock().unwrap().scope,
    )))
}

pub fn install_stdlib(frame: &mut Frame) {
    use crate::stdlib as lib;

    fn register<V: Into<Value>>(frame: &mut Frame, name: &str, value: V) {
        frame.set_var(name.to_owned(), value.into());
    }

    fn register_in_ns<V: Into<Value>>(ns: &mut HashMap<String, Value>, name: &str, value: V) {
        ns.insert(name.to_owned(), value.into());
    }

    fn namespace(frame: &mut Frame, name: &str, factory: fn(&mut HashMap<String, Value>)) {
        let mut ns: HashMap<String, Value> = HashMap::new();
        factory(&mut ns);
        frame.set_var(name.to_owned(), ns.into());
    }

    register::<NativeFunctionDef>(frame, "typeof", ("typeof", lib::type_of));

    #[cfg(debug_assertions)]
    register::<NativeFunctionDef>(
        frame,
        "inspectScopeChain",
        ("inspectScopeChain", inspect_scope_chain_binding),
    );

    #[cfg(debug_assertions)]
    register::<NativeFunctionDef>(
        frame,
        "inspectScope",
        ("inspectScope", inspect_scope_binding),
    );

    register::<NativeFunctionDef>(frame, "map", ("map", lib::map));
    register::<NativeFunctionDef>(frame, "reduce", ("reduce", lib::reduce));
    register::<NativeFunctionDef>(frame, "seq", ("seq", lib::seq));
    register::<NativeFunctionDef>(frame, "concat", ("concat", lib::concat));

    namespace(frame, "console", |mut ns| {
        register_in_ns::<NativeFunctionDef>(&mut ns, "log", ("log", lib::console::log));
        register_in_ns::<NativeFunctionDef>(&mut ns, "dir", ("dir", lib::console::dir));
        register_in_ns::<NativeFunctionDef>(&mut ns, "inspect", ("inspect", lib::console::inspect));
    });

    namespace(frame, "math", |mut ns| {
        register_in_ns::<NativeFunctionDef>(&mut ns, "sum", ("sum", lib::math::sum));
        register_in_ns::<NativeFunctionDef>(&mut ns, "greatest", ("greatest", lib::math::greatest));
        register_in_ns::<NativeFunctionDef>(&mut ns, "smallest", ("smallest", lib::math::smallest));
    });
}

#[derive(Debug, Clone)]
pub struct Closure {
    name: String,
    scope_chain: Vec<ScopeRef>,
    slots: Vec<ast::Slot>,
    expression: ast::Expression,
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.name == "anonymous" {
            write!(f, "(")?;
            write_list(f, &self.slots, ", ")?;
            write!(f, ") => {}", self.expression)
        } else {
            write!(f, "function {}(", self.name)?;
            write_list(f, &self.slots, ", ")?;
            write!(f, ") {}", self.expression)
        }
    }
}

type NativeFunctionBinding = fn(Vec<Value>, &mut Context) -> Result<Value, Value>;
type NativeFunctionDef = (&'static str, NativeFunctionBinding);

#[derive(Clone)]
pub struct NativeFunction {
    binding: NativeFunctionBinding,
    name: &'static str,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("func", &"[internal]")
            .field("name", &self.name)
            .finish()
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function {}() {{ [native code] }}", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    object: Value,
    method: Function,
    name: String,
}

impl Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function {}() {{ [native code] }}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    Closure(Box<Closure>),
    NativeFunction(Box<NativeFunction>),
    Binding(Box<Binding>),
}

impl Function {
    fn print(&self) -> String {
        match self {
            Function::Closure(value) => format!("[Function: {}]", value.name),
            Function::NativeFunction(value) => format!("[Function: {} (native)]", value.name),
            Function::Binding(value) => format!("[Function: {} (bound)]", value.name),
        }
    }

    fn name(&self) -> &str {
        match self {
            Function::Closure(value) => &value.name,
            Function::NativeFunction(value) => value.name,
            Function::Binding(value) => &value.name,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Closure(v) => write!(f, "{}", v),
            Function::NativeFunction(v) => write!(f, "{}", v),
            Function::Binding(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Void,
    Number(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Function(Function),
}

impl Value {
    pub fn get_type(&self) -> String {
        match self {
            Value::Void => String::from("Void"),
            Value::Number(_) => String::from("Number"),
            Value::String(_) => String::from("String"),
            Value::Boolean(_) => String::from("Boolean"),
            Value::List(_) => String::from("List"),
            Value::Map(_) => String::from("Map"),
            Value::Function(_) => String::from("Function"),
        }
    }

    /// Used for logging detailed information about the value for debugging
    pub fn inspect(&self) -> String {
        match self {
            Value::Void => self.get_type(),
            _ => format!("{}({})", self.get_type(), self),
        }
    }

    /// Used for printing the value to a terminal
    /// E.g. in a REPL
    pub fn print(&self, depth: usize) -> colored::ColoredString {
        use colored::*;

        match self {
            Value::Void => format!("{}", self).dimmed(),
            Value::Number(value) => format!("{}", value).yellow(),
            Value::String(value) => format!("\"{}\"", value).green(),
            Value::Boolean(value) => format!("{}", value).yellow(),
            Value::List(value) => format!(
                "[{}]",
                value
                    .iter()
                    .map(|v| v.print(depth).to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            )
            .normal(),
            Value::Function(value) => value.print().blue(),
            Value::Map(value) => format!(
                "{{\n{}\n{}}}",
                value
                    .iter()
                    .map(|(k, v)| {
                        let depth = depth + 1;

                        format!("{}{}: {}", "  ".repeat(depth), k, v.print(depth))
                    })
                    .collect::<Vec<String>>()
                    .join(",\n"),
                "  ".repeat(depth),
            )
            .normal(),
        }
    }

    pub fn as_number(&self) -> Result<f64, Value> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(Value::from(format!(
                "TypeError: {} can not be represented as number",
                self.inspect()
            ))),
        }
    }

    pub fn as_boolean(&self) -> Result<bool, Value> {
        match self {
            Value::Void => Ok(false),
            Value::Number(n) => Ok(*n != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
            Value::Boolean(b) => Ok(*b),
            _ => Ok(true),
        }
    }

    pub fn equals(&self, other: &Value) -> bool {
        match self {
            Value::Void => matches!(other, Value::Void),
            Value::Number(own) => match other {
                Value::Number(other) => own == other,
                _ => false,
            },
            Value::String(own) => match other {
                Value::String(other) => own == other,
                _ => false,
            },
            Value::Boolean(own) => match other {
                Value::Boolean(other) => own == other,
                _ => false,
            },

            // TODO: Implement equal for complex types
            Value::Function(_) | Value::Map(_) | Value::List(_) => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // IDEA: Should the runtime and rust impl be the same?
        self.equals(other)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Number(value) => {
                if *value == std::f64::INFINITY {
                    write!(f, "Infinity")
                } else if *value == -std::f64::INFINITY {
                    write!(f, "-Infinity")
                } else if value.is_nan() {
                    write!(f, "NaN")
                } else {
                    write!(f, "{}", value)
                }
            }
            Value::String(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::List(value) => write!(
                f,
                "{}",
                value
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Map(value) => write!(f, "{:?}", value),
            Value::Function(value) => write!(f, "{}", value),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Void
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Value {
        Value::Void
    }
}

impl From<f64> for Value {
    fn from(number: f64) -> Value {
        Value::Number(number)
    }
}

impl From<isize> for Value {
    fn from(number: isize) -> Value {
        Value::Number(number as f64)
    }
}

impl From<i32> for Value {
    fn from(number: i32) -> Value {
        Value::Number(number as f64)
    }
}

impl From<i64> for Value {
    fn from(number: i64) -> Value {
        Value::Number(number as f64)
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

impl From<bool> for Value {
    fn from(boolean: bool) -> Value {
        Value::Boolean(boolean)
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

impl From<Function> for Value {
    fn from(func: Function) -> Value {
        Value::Function(func)
    }
}

impl From<Closure> for Value {
    fn from(closure: Closure) -> Value {
        Value::from(Function::Closure(Box::new(closure)))
    }
}

impl From<NativeFunction> for Value {
    fn from(nf: NativeFunction) -> Value {
        Value::from(Function::NativeFunction(Box::new(nf)))
    }
}

impl From<NativeFunctionDef> for Value {
    fn from((name, binding): NativeFunctionDef) -> Value {
        let nf = NativeFunction { name, binding };
        Value::from(nf)
    }
}

impl From<Binding> for Value {
    fn from(binding: Binding) -> Value {
        Value::from(Function::Binding(Box::new(binding)))
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

    #[cfg(debug_assertions)]
    fn print(&self) {
        println!("{{");
        for (key, value) in &self.vars {
            println!("  {}: {}", key, value.print(1));
        }
        println!("}}");
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    scope: ScopeRef,
    scope_chain: Vec<ScopeRefWeak>,
}

impl Frame {
    fn new() -> Self {
        Self {
            scope: Arc::new(Mutex::new(Scope::new())),
            scope_chain: vec![],
        }
    }

    // A closure holds hard refs to its scope but in a frame we need weak refs
    fn from_closure(scope_chain: &[ScopeRef]) -> Self {
        Self {
            scope: Arc::new(Mutex::new(Scope::new())),
            scope_chain: scope_chain
                .iter()
                .map(Arc::downgrade)
                .collect::<Vec<ScopeRefWeak>>(),
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        let value = {
            let scope = self.scope.lock().unwrap();

            scope.get(name)
        };

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
        let mut frame = Frame::new();

        install_stdlib(&mut frame);

        Self {
            current: Arc::new(Mutex::new(frame)),
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

    // The frame only has weak refs to its scope chain but we export to create a closure with hard refs
    fn export_scope_chain(&self) -> Vec<ScopeRef> {
        let current = self.current.lock().unwrap();

        let mut chain = current
            .scope_chain
            .iter()
            .map(|scope| scope.upgrade().expect("Scope already dropped"))
            .collect::<Vec<ScopeRef>>();

        chain.push(current.scope.clone());

        chain
    }
}

fn call_func(func: &Function, mut args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    match func {
        Function::Closure(closure) => {
            // The closure frame only has a weak ref to the scope so it is very important that the owned scope chain lives until the closure is evaluated
            let frame = Frame::from_closure(&closure.scope_chain);
            let frame = Arc::new(Mutex::new(frame));

            ctx.push_stack(frame);

            for (i, arg) in args.drain(..).enumerate() {
                if i < closure.slots.len() {
                    ctx.set_var(&closure.slots[i].id, arg);
                } else {
                    break;
                }
            }

            let result = eval_expr(&closure.expression, ctx);

            ctx.pop_stack();

            result
        }

        Function::Binding(binding) => call_func(
            &binding.method,
            [vec![binding.object.to_owned()], args].concat(),
            ctx,
        ),

        Function::NativeFunction(nf) => (nf.binding)(args, ctx),
    }
}

pub fn call_value(callee: &Value, args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    match callee {
        Value::Function(func) => call_func(func, args, ctx),

        _ => Err(Value::from(format!(
            "TypeError: {} is not a function",
            callee.inspect()
        ))),
    }
}

fn eval_expr_list(expr_list: &[ast::Expression], ctx: &mut Context) -> Result<Value, Value> {
    let mut final_val = Value::Void;
    for expr in expr_list {
        final_val = eval_expr(&expr, ctx)?;
    }
    Ok(final_val)
}

fn eval_expr(expr: &ast::Expression, ctx: &mut Context) -> Result<Value, Value> {
    use ast::Expression::*;

    match expr {
        Call(call) => {
            let callee = eval_expr(&call.callee, ctx)?;

            let args = call
                .arguments
                .iter()
                .map(|expr| eval_expr(&expr, ctx))
                .collect::<Result<Vec<Value>, Value>>()?;

            call_value(&callee, args, ctx)
        }
        Binary(binary) => {
            use ast::BinaryOperator::*;

            let left = eval_expr(&binary.left, ctx)?;

            let value = match binary.op {
                Add => Value::from(left.as_number()? + eval_expr(&binary.right, ctx)?.as_number()?),
                Sub => Value::from(left.as_number()? - eval_expr(&binary.right, ctx)?.as_number()?),
                Mul => Value::from(left.as_number()? * eval_expr(&binary.right, ctx)?.as_number()?),
                Div => Value::from(left.as_number()? / eval_expr(&binary.right, ctx)?.as_number()?),
                Or => {
                    if left.as_boolean()? {
                        left
                    } else {
                        eval_expr(&binary.right, ctx)?
                    }
                }
                And => {
                    if left.as_boolean()? {
                        eval_expr(&binary.right, ctx)?
                    } else {
                        left
                    }
                }
                Eq => Value::from(left.equals(&eval_expr(&binary.right, ctx)?)),
                Ne => Value::from(!left.equals(&eval_expr(&binary.right, ctx)?)),
                Gt => Value::from(left.as_number()? > eval_expr(&binary.right, ctx)?.as_number()?),
                Lt => Value::from(left.as_number()? < eval_expr(&binary.right, ctx)?.as_number()?),
                Ge => Value::from(left.as_number()? >= eval_expr(&binary.right, ctx)?.as_number()?),
                Le => Value::from(left.as_number()? <= eval_expr(&binary.right, ctx)?.as_number()?),
                Concat => Value::from(format!("{}{}", left, eval_expr(&binary.right, ctx)?)),
            };

            Ok(value)
        }
        Unary(unary) => {
            use ast::UnaryOperator::*;

            let expr = eval_expr(&unary.expr, ctx)?;

            let value = match unary.op {
                Not => Value::from(!expr.as_boolean()?),
                Neg => Value::from(-expr.as_number()?),
                Pos => Value::from(expr.as_number()?),
            };

            Ok(value)
        }
        MemberAccess(member_access) => {
            let prop = &member_access.property.name;
            let object = eval_expr(&member_access.object, ctx)?;

            match object {
                Value::Map(map) => Ok(map.get(prop).cloned().unwrap_or_default()),
                _ => Err(Value::from(format!(
                    "TypeError: Can not access property '{}' of {}",
                    prop,
                    object.inspect()
                ))),
            }
        }
        Bind(bind) => {
            let object = eval_expr(&bind.object, ctx)?;
            let method = eval_expr(&bind.method, ctx)?;

            match method {
                Value::Function(func) => {
                    let name = func.name().to_owned();

                    Ok(Value::from(Binding {
                        object,
                        method: func,
                        name,
                    }))
                }
                _ => Err(Value::from(format!(
                    "TypeError: Can not bind non function {}",
                    method.get_type()
                ))),
            }
        }
        Declaration(declaration) => {
            let value = eval_expr(&declaration.value, ctx)?;
            ctx.set_var(&declaration.id, value.clone());
            Ok(value)
        }
        Block(block) => eval_expr_list(&block.body, ctx),
        IfElse(if_else) => {
            if eval_expr(&if_else.condition, ctx)?.as_boolean()? {
                eval_expr(&if_else.then, ctx)
            } else {
                // TODO: Change this to None when we get Optionals
                if_else
                    .r#else
                    .as_ref()
                    .map(|expr| eval_expr(expr, ctx))
                    .unwrap_or_else(|| Ok(Value::Void))
            }
        }
        Id(id) => ctx
            .get_var(&id)
            .ok_or_else(|| Value::from(format!("ReferenceError: {} is not defined", id))),
        VoidLiteral(_) => Ok(Value::Void),
        NumberLiteral(number_literal) => Ok(Value::from(number_literal.value.to_owned())),
        StringLiteral(string_literal) => Ok(Value::from(string_literal.value.to_owned())),
        ListLiteral(list_literal) => {
            let results: Result<Vec<Value>, Value> = list_literal
                .values
                .iter()
                .map(|expr| eval_expr(expr, ctx))
                .collect();

            results.map(Value::from)
        }
        BooleanLiteral(boolean_literal) => Ok(Value::from(boolean_literal.value)),
        Function(function) => {
            let value = Value::from(Closure {
                name: function.id.to_owned().name,
                expression: function.expression.to_owned(),
                slots: function.slots.to_owned(),
                scope_chain: ctx.export_scope_chain(),
            });
            ctx.set_var(&function.id, value.clone());
            Ok(value)
        }
        Lambda(function) => Ok(Value::from(Closure {
            name: "anonymous".to_owned(),
            expression: function.expression.to_owned(),
            slots: function.slots.to_owned(),
            scope_chain: ctx.export_scope_chain(),
        })),
    }
}

pub fn exec_with_context(program: &ast::Program, ctx: &mut Context) -> Result<Value, Value> {
    eval_expr_list(&program.body, ctx)
}

pub fn exec(program: &ast::Program) -> Result<Value, Value> {
    let mut context = Context::new();
    eval_expr_list(&program.body, &mut context)
}

#[cfg(test)]
mod tests {
    use super::{exec_with_context, Context, Value};
    use crate::parser;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_var_def_and_access() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let foo = 1;").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::Number(1.0));

        let ast = parser::parse_string("foo;").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::Number(1.0));
    }

    #[test]
    fn test_function_def_and_call() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("function addOne(arg) { arg + 1 };").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("addOne(1);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::Number(2.0));
    }

    #[test]
    fn test_lambda_functions() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let add = (a) => (b) => a + b").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("let add2 = add(2);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("add2(1);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(3.0));
    }

    #[test]
    fn test_currying_call_style() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let add = (a) => (b) => a + b").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("add(2)(1);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(3.0));
    }

    #[test]
    fn test_currying_call_style_2() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let add3 = (a) => (b) => (c) => a + b + c").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("add3(2)(1)(1);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(4.0));
    }

    #[test]
    fn test_call_with_too_many_args() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let f = (a) => a").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("f(1, 2, 3);").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(1.0));
    }

    #[test]
    fn test_void_equals_void() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("void == void").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from(true));
    }

    #[test]
    fn test_neg_zero_equals_zero() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("-0 == 0").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from(true));
    }

    #[test]
    fn test_nan_equal_nan() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("NaN == NaN").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from(false));
    }

    #[test]
    fn test_nan_not_equal_nan() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("NaN != NaN").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from(true));
    }

    #[test]
    fn test_nan_to_string() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("NaN ++ ''").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from("NaN"));
    }

    #[test]
    fn test_neg_nan_to_string() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("-NaN ++ ''").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from("NaN"));
    }

    #[test]
    fn test_infinity_to_string() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("Infinity ++ ''").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from("Infinity"));
    }

    #[test]
    fn test_neg_infinity_to_string() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("-Infinity ++ ''").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from("-Infinity"));
    }

    #[test]
    fn test_neg_zero_to_string() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("-0 ++ ''").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();
        assert_eq!(result, Value::from("0"));
    }

    #[test]
    fn test_bind() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let add = (a, b) => a + b").unwrap();
        exec_with_context(&ast, &mut ctx).unwrap();

        let ast = parser::parse_string("let add2 = 2:add").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result.get_type(), "Function");

        let ast = parser::parse_string("add2(2)").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(4));
    }

    #[test]
    fn test_bind_direct_call() {
        let mut ctx = Context::new();

        let ast = parser::parse_string("let add = (a, b) => a + b").unwrap();
        exec_with_context(&ast, &mut ctx).unwrap();

        let ast = parser::parse_string("2:add(2)").unwrap();
        let result = exec_with_context(&ast, &mut ctx).unwrap();

        assert_eq!(result, Value::from(4));
    }
}
