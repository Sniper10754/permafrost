use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use crate::interpreter::{Interpreter, Value};

fn print(args: Vec<Rc<Value<'_>>>) -> Value<'_> {
    let mut buf = String::new();
    let mut fmt = Formatter::new(&mut buf);

    for (index, arg) in args.iter().enumerate() {
        Display::fmt(arg, &mut fmt).unwrap();

        if index != args.len() - 1 {
            write!(fmt, ", ").unwrap();
        }
    }

    println!("{buf}");

    Value::Nothing
}

pub fn insert_intrinsics(interpreter: &mut Interpreter<'_>) {
    let intrinsics_functions = interpreter.intrinsic_functions_mut();

    intrinsics_functions.insert("print", Box::new(print));
}
