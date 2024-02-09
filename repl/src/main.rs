use color_eyre::eyre;
use permafrost_compiler::{context::CompilerContext, CompilerError};
use permafrost_reports::printer::{DefaultPrintBackend, ReportPrinter};
use permafrost_runtime::Runtime;
use permafrost_vm_core::value::Value;
use rustyline::DefaultEditor;

#[derive(Default)]
pub struct Repl
{
    runtime: Runtime,
}

impl Repl
{
    pub fn run_code(
        &mut self,
        ctx: &mut CompilerContext,
        code: &str,
    ) -> Result<Value, CompilerError>
    {
        self.runtime.eval_code(ctx, "REPL", code)
    }
}

fn main() -> eyre::Result<()>
{
    color_eyre::install()?;

    let mut line_editor = DefaultEditor::new()?;

    let mut repl = Repl::default();

    let mut code_buffer = String::default();

    for buf_result in line_editor.iter(">> ") {
        let buffer = buf_result?;

        let mut ctx = CompilerContext::new();

        match repl.run_code(&mut ctx, &buffer) {
            Ok(value) => {
                println!("{value}")
            }
            Err(_context) => {
                code_buffer.clear();

                ReportPrinter::new(&mut code_buffer)
                    .print_reports::<DefaultPrintBackend, _>(&ctx.src_map, &*ctx.report_ctx)?;

                println!("{code_buffer}")
            }
        };

        ctx.clear();

        code_buffer.clear();
    }

    Ok(())
}
