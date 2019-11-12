use std::{fmt, fmt::Display};

pub fn write_list<W: fmt::Write, T: Display>(
    f: &mut W,
    list: &[T],
    seperator: &str,
) -> fmt::Result {
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
