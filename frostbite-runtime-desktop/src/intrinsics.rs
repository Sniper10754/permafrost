use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use frostbite_runtime::{value::Value, Runtime};

fn print<'args>(args: &[Rc<Value<'_>>]) -> Value<'args> {
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

pub fn insert_intrinsics(runtime: &mut Runtime<'_>) {
    let intrinsics_functions = runtime.intrinsic_functions_mut();

    intrinsics_functions.insert("print", Box::new(print));
}
