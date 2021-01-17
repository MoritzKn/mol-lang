use crate::interpreter::{Context, Value};

pub fn type_of(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
    args.get(0)
        .map(|v| Value::from(v.get_type()))
        .ok_or_else(|| Value::from("TypeError: Expected one argument but got 0"))
}

pub mod console {
    use crate::interpreter::{Context, Value};

    pub fn log(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        let text = args
            .iter()
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
            .iter()
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
        args.iter()
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
        args.iter()
            .try_fold(f64::NEG_INFINITY, |acc, curr| match curr {
                Value::Number(curr) => Ok(if acc < *curr { *curr } else { acc }),
                _ => Err(Value::from(format!(
                    "TypeError: Can not find greatest for element of type {}",
                    curr.get_type()
                ))),
            })
            .map(Value::Number)
    }

    pub fn smallest(args: Vec<Value>, _ctx: &mut Context) -> Result<Value, Value> {
        args.iter()
            .try_fold(f64::INFINITY, |acc, curr| match curr {
                Value::Number(curr) => Ok(if acc > *curr { *curr } else { acc }),
                _ => Err(Value::from(format!(
                    "TypeError: Can not find smallest for element of type {}",
                    curr.get_type()
                ))),
            })
            .map(Value::Number)
    }
}
