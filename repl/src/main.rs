use frostbite_compiler::context::CompilerContext;
use frostbite_reports::{
    printer::{DefaultPrintBackend, ReportPrinter},
    Report,
};
use frostbite_runtime::Runtime;
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};
use std::error::Error;

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
    ) -> Result<(), CompilerContext>
    {
        self.runtime.eval_code(code)
    }
}

fn main() -> Result<(), Box<dyn Error>>
{
    // Create a default reedline object to handle user input

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
                    Ok(_) => {}
                    Err(context) => {
                        code_buffer.clear();

                        ReportPrinter::new(&mut code_buffer)
                            .print_reports::<DefaultPrintBackend, _>(
                                &context.src_map,
                                &*context.report_ctx,
                            )?;
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
