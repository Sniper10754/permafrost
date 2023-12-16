use std::fmt::{Display, Formatter};

use frostbite_runtime::{internals::Shared, value::Value, Runtime};

fn print<'id, 'args>(args: &[Shared<Value<'id, '_>>]) -> Value<'id, 'args> {
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

pub fn insert_intrinsics(runtime: &mut Runtime<'_, '_, '_>) {
    let intrinsics_ctx = runtime.intrinsic_ctx();

    intrinsics_ctx
        .intrinsic_functions
        .insert("print", Box::new(print));
}
