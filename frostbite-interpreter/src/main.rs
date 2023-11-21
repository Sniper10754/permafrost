mod interpreter;
mod rt_value;
mod helper;

use std::{env::args, error::Error, fs, path::PathBuf, process};

use frostbite_parser::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    let mut args = args();

    if args.len() != 2 {
        println!("usage: frostbite-interpreter [PATH]");

        process::exit(1);
    }

    let path = PathBuf::from(args.nth(1).unwrap());
    let content = fs::read_to_string(path)?;

    let ast = Parser::new().parse(&content);

    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => {
            return Err(format!("parser: {err:?}").into());
        }
    };

    interpreter::Interpreter::default().run(&ast);

    Ok(())
}
<<<<<<< HEAD
=======

#[derive(Debug, Clone)]
pub enum RuntimeValue<'input> {
    Int(i32),
    Float(f32),
    Ident(&'input str),
    String(String),

    Unit,
}

#[derive(Debug, Clone, Default)]
struct Interpreter<'input> {
    frames: Vec<StackFrame<'input>>,
}

#[derive(Debug, Clone)]
struct StackFrame<'input> {
    name: Cow<'input, str>,
    stack: BTreeMap<&'input str, RuntimeValue<'input>>,
}

impl<'input> Interpreter<'input> {
    fn run(mut self, program: &Program<'input>) {
        for expr in &program.exprs {
            match self.eval_expr(expr) {
                Ok(value) => {
                    dbg!(value);
                }
                Err(err) => {
                    println!("{err:?}");

                    break;
                }
            }
        }
    }

    fn eval_expr<'a>(&mut self, expr: &'a Expr<'input>) -> Result<RuntimeValue<'input>, Report> {
        match expr {
            Expr::Int(_, int) => Ok(RuntimeValue::Int(*int)),
            Expr::Float(_, float) => Ok(RuntimeValue::Float(*float)),
            Expr::Ident(_, ident) => Ok(RuntimeValue::Ident(ident)),
            Expr::String(_, string_slice) => Ok(RuntimeValue::String(string_slice.to_string())),
            Expr::BinaryOperation { lhs, operator, rhs } => {
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;

                match (lhs, rhs) {
                    (RuntimeValue::Int(lhs), RuntimeValue::Int(rhs)) => match operator {
                        Add => Ok(RuntimeValue::Int(lhs + rhs)),
                        Sub => Ok(RuntimeValue::Int(lhs - rhs)),
                        Mul => Ok(RuntimeValue::Int(lhs * rhs)),
                        Div => Ok(RuntimeValue::Int(lhs / rhs)),
                    },
                    (RuntimeValue::Float(lhs), RuntimeValue::Float(rhs)) => match operator {
                        Add => Ok(RuntimeValue::Float(lhs + rhs)),
                        Sub => Ok(RuntimeValue::Float(lhs - rhs)),
                        Mul => Ok(RuntimeValue::Float(lhs * rhs)),
                        Div => Ok(RuntimeValue::Float(lhs / rhs)),
                    },

                    (lhs, rhs) => Err(Report {
                        level: Level::Error,
                        location: Some(expr.span()),
                        title: "Incompatible operands".into(),
                        description: Some(
                            format!(
                                "`{:?}` and `{:?}` cannot be used with operand `{operator}`",
                                lhs, rhs
                            )
                            .into(),
                        ),
                        infos: vec![],
                        helps: vec![],
                    }),
                }
            }
            Expr::Assign { lhs, value } => {
                if !matches!(&**lhs, Expr::Ident(..)) {
                    return Err(Report {
                        level: Level::Error,
                        location: Some(lhs.span()),
                        title: "Cannot assign to static/immutable".into(),
                        description: Some(format!("Cannot assign {value:?} to {lhs:?}").into()),
                        infos: vec![],
                        helps: vec![],
                    });
                }
                match &**lhs {
                    Expr::Ident(_, ident) => {
                        let value = self.eval_expr(value)?;

                        let stack_frame_maybe_contaning_var = self
                            .frames
                            .iter_mut()
                            .find(|frame| frame.stack.contains_key(ident));

                        if let Some(frame) = stack_frame_maybe_contaning_var {
                            frame.stack.insert(ident, value);
                        } else {
                            self.frames.first_mut().unwrap().stack.insert(ident, value);
                        }
                    }

                    _ => todo!(),
                }

                Ok(RuntimeValue::Unit)
            }
        }
    }
}
>>>>>>> origin/master
