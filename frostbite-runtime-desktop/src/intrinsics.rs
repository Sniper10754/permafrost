use std::fmt::{Display, Formatter};

use frostbite_sandbox::{internals::Shared, intrinsic::IntrinsicContext, value::Value};

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

pub fn insert_intrinsics(intrinsics_ctx: &mut IntrinsicContext<'_, '_>) {
    intrinsics_ctx
        .intrinsic_functions
        .insert("print", Box::new(print));
}
