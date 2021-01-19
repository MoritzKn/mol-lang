use crate::interpreter::{eval_func, Context, Value};

pub fn type_of(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    args.into_iter()
        .next()
        .map(|v| Value::from(v.get_type()))
        .ok_or_else(|| Value::from("TypeError: Expected 1 argument but got 0"))
}

pub fn seq(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter().map(|v| match v {
        Value::Number(n) if n <= 9007199254740991.0 => Ok(n as i64),
        Value::Number(n) => Err(Value::from(format!(
            "TypeError: Number out of integer range {}",
            n
        ))),
        _ => Err(Value::from(format!(
            "TypeError: Expected number but got {}",
            v.get_type()
        ))),
    });

    let lower = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 3 argument but got 0"))??;

    let upper = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 2 argument but got 1"))??;

    let list: Vec<Value> = (lower..upper).into_iter().map(Value::from).collect();

    Ok(Value::from(list))
}

pub fn map(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected two argument but got 0"))
        .and_then(|v| match v {
            Value::List(list) => Ok(list),
            _ => Err(Value::from(format!(
                "TypeError: Expected list but got {}",
                v.get_type()
            ))),
        })?;

    let callback = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected two argument but got 1"))
        .and_then(|v| match v {
            Value::Function(_) | Value::NativeFunction(_) => Ok(v),
            _ => Err(Value::from(format!(
                "TypeError: Expected function but got {}",
                v.get_type()
            ))),
        })?;

    list.into_iter()
        .map(|value| eval_func(&callback, vec![value], ctx))
        .collect::<Result<Vec<Value>, Value>>()
        .map(Value::from)
}

pub fn reduce(args: Vec<Value>, ctx: &mut Context) -> Result<Value, Value> {
    let mut iter = args.into_iter();

    let list = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 3 argument but got 0"))
        .and_then(|v| match v {
            Value::List(list) => Ok(list),
            _ => Err(Value::from(format!(
                "TypeError: Expected list but got {}",
                v.get_type()
            ))),
        })?;

    let callback = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 3 argument but only got 1"))
        .and_then(|v| match v {
            Value::Function(_) | Value::NativeFunction(_) => Ok(v),
            _ => Err(Value::from(format!(
                "TypeError: Expected function but got {}",
                v.get_type()
            ))),
        })?;

    let inital = iter
        .next()
        .ok_or_else(|| Value::from("TypeError: Expected 3 argument but only got 2"))?;

    list.into_iter().try_fold(inital, |acc, curr| {
        eval_func(&callback, vec![acc, curr], ctx)
    })
}

pub fn concat(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    let list: Vec<Value> = args.into_iter().fold(vec![], |mut list, v| match v {
        Value::List(list_in_arg) => [list, list_in_arg].concat(),
        _ => {
            list.push(v);
            list
        }
    });

    Ok(Value::from(list))
}

pub mod console {
    use crate::interpreter::{Context, Value};

    pub fn log(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        let text = args
            .into_iter()
            .map(|v| v.to_string())
            .collect::<Vec<String>>()
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
        let text = args
            .into_iter()
            .map(|v| v.inspect())
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", text);

        Ok(Value::Void)
    }
}

pub mod math {
    use crate::interpreter::{Context, Value};
    use std::f64;

    pub fn sum(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .try_fold(0.0, |acc, curr| match curr {
                Value::Number(curr) => Ok(acc + curr),
                _ => Err(Value::from(format!(
                    "TypeError: Can not sum elements of type {}",
                    curr.get_type()
                ))),
            })
            .map(Value::Number)
    }

    pub fn greatest(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .try_fold(f64::NEG_INFINITY, |acc, curr| match curr {
                Value::Number(curr) => Ok(if acc < curr { curr } else { acc }),
                _ => Err(Value::from(format!(
                    "TypeError: Can not find greatest for element of type {}",
                    curr.get_type()
                ))),
            })
            .map(Value::Number)
    }

    pub fn smallest(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.into_iter()
            .try_fold(f64::INFINITY, |acc, curr| match curr {
                Value::Number(curr) => Ok(if acc > curr { curr } else { acc }),
                _ => Err(Value::from(format!(
                    "TypeError: Can not find smallest for element of type {}",
                    curr.get_type()
                ))),
            })
            .map(Value::Number)
    }
}
