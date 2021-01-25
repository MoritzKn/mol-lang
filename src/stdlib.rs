use crate::interpreter::{call_func, Context, Value};

fn arg_error(expected: usize, found: usize) -> Value {
    Value::from(format!(
        "TypeError: Expected {} arguments but got {}",
        expected, found
    ))
}

pub fn type_of(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    args.into_iter()
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 1 argument but got 0"))
        .map(|v| Value::from(v.get_type()))
}

pub fn seq(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let lower = iter
        .next()
        .ok_or(arg_error(2, 0))
        .and_then(|v| v.as_integer())?;
    let upper = iter
        .next()
        .ok_or(arg_error(2, 1))
        .and_then(|v| v.as_integer())?;

    let list: Vec<Value> = (lower..upper).into_iter().map(Value::from).collect();

    Ok(Value::from(list))
}

pub fn map(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or(arg_error(2, 0))
        .and_then(|v| v.as_list())?;
    let callback = iter
        .next()
        .ok_or(arg_error(2, 1))
        .and_then(|v| v.as_function())?;

    list.into_iter()
        .map(|value| call_func(&callback, vec![value], ctx))
        .collect::<Result<Vec<Value>, Value>>()
        .map(Value::from)
}

pub fn for_each(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or(arg_error(2, 0))
        .and_then(|v| v.as_list())?;
    let callback = iter
        .next()
        .ok_or(arg_error(2, 1))
        .and_then(|v| v.as_function())?;

    for item in list {
        call_func(&callback, vec![item.clone()], ctx)?.as_boolean()?;
    }

    Ok(Value::Void)
}

pub fn filter(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or(arg_error(2, 0))
        .and_then(|v| v.as_list())?;
    let callback = iter
        .next()
        .ok_or(arg_error(2, 1))
        .and_then(|v| v.as_function())?;

    let mut out = vec![];
    for item in list {
        let res = call_func(&callback, vec![item.clone()], ctx)?;
        if res.as_boolean()? {
            out.push(item);
        }
    }

    Ok(Value::from(out))
}

pub fn reduce(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or(arg_error(3, 0))
        .and_then(|v| v.as_list())?;
    let callback = iter
        .next()
        .ok_or(arg_error(3, 1))
        .and_then(|v| v.as_function())?;
    let inital = iter.next().ok_or(arg_error(3, 2))?;

    list.into_iter().try_fold(inital, |acc, curr| {
        call_func(&callback, vec![acc, curr], ctx)
    })
}

pub fn concat(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    let list: Vec<Value> = args.into_iter().fold(vec![], |list, v| {
        [
            list,
            match v {
                Value::List(v) => v,
                _ => vec![v],
            },
        ]
        .concat()
    });

    Ok(Value::from(list))
}

pub mod console {
    use super::*;

    pub fn log(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        let text = args
            .into_iter()
            .map(|v| v.as_string())
            .collect::<Result<Vec<String>, Value>>()?
            .join(" ");

        println!("{}", text);

        Ok(Value::Void)
    }

    pub fn dir(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        for value in args {
            println!("{}", value.print(0));
        }

        Ok(Value::Void)
    }

    pub fn inspect(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        for arg in args {
            print!("{} ", arg.print(0));
        }
        println!("");

        Ok(Value::Void)
    }
}

pub mod math {
    use super::*;
    use std::f64;

    pub fn sum(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .map(|v| v.as_number())
            .try_fold(0.0, |acc, curr| curr.map(|num| num + acc))
            .map(Value::from)
    }

    pub fn greatest(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .map(|v| v.as_number())
            .try_fold(f64::NEG_INFINITY, |acc, curr| {
                curr.map(|curr| if acc < curr { curr } else { acc })
            })
            .map(Value::from)
    }

    pub fn smallest(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .map(|v| v.as_number())
            .try_fold(f64::NEG_INFINITY, |acc, curr| {
                curr.map(|curr| if acc > curr { curr } else { acc })
            })
            .map(Value::from)
    }

    pub fn random(_args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        let value: f64 = rand::random();

        Ok(Value::from(value))
    }

    pub fn round(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        let n = args
            .into_iter()
            .next()
            .ok_or(arg_error(1, 0))
            .and_then(|v| v.as_number())?;

        Ok(Value::from(n.round()))
    }
}
