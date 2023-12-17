use std::fmt::{Display, Formatter};

use frostbite_runtime::{internals::Shared, intrinsic::IntrinsicContext, value::Value};

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

pub fn insert_intrinsics(intrinsic_ctx: &mut IntrinsicContext<'_, '_>) {
    intrinsic_ctx
        .intrinsic_functions
        .insert("print", Shared::new(print));
}
