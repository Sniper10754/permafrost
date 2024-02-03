use color_eyre::eyre;
use frostbite_compiler::context::CompilerContext;
use frostbite_reports::printer::{DefaultPrintBackend, ReportPrinter};
use frostbite_runtime::Runtime;
use frostbite_vm_core::value::Value;
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};

#[derive(Default)]
pub struct Repl
{
    runtime: Runtime,
}

impl Repl
{
    pub fn run_code(
        &mut self,
        code: &str,
    ) -> Result<Value, CompilerContext>
    {
        self.runtime.eval_code("REPL", code)
    }
}

fn main() -> eyre::Result<()>
{
    color_eyre::install()?;

    let mut line_editor = Reedline::create()
        .use_kitty_keyboard_enhancement(true)
        .with_quick_completions(true);

    let prompt = DefaultPrompt {
        left_prompt: DefaultPromptSegment::Basic("REPL".into()),
        right_prompt: DefaultPromptSegment::CurrentDateTime,
    };

    let mut repl = Repl::default();

    let mut code_buffer = String::default();

    loop {
        let sig = line_editor.read_line(&prompt)?;

        match sig {
            Signal::Success(buffer) => {
                match repl.run_code(&buffer) {
                    Ok(value) => {
                        println!("{value}")
                    }
                    Err(context) => {
                        code_buffer.clear();

                        ReportPrinter::new(&mut code_buffer)
                            .print_reports::<DefaultPrintBackend, _>(
                                &context.src_map,
                                &*context.report_ctx,
                            )?;

                        println!("{code_buffer}")
                    }
                };
            }

            Signal::CtrlD | Signal::CtrlC => {
                println!("Aborted");
                break;
            }
        }

        code_buffer.clear();
    }

    Ok(())
}
