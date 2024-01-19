use frostbite_bytecode::text_repr::print_bytecode;
use frostbite_compiler::{
    codegen::CodegenBackends, context::CompilerContext, CompilationResults, Compiler,
};
use frostbite_reports::printer::{DefaultPrintBackend, ReportPrinter};
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};
use std::error::Error;

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

    loop {
        let sig = line_editor.read_line(&prompt)?;

        let mut code_buffer = String::default();

        match sig {
            Signal::Success(buffer) => {
                let bytecode = compile_code(&buffer);

                match bytecode {
                    Ok(module) => {
                        print_bytecode(&mut code_buffer, &module)?;
                    }
                    Err(compiler_ctx) => {
                        compiler_ctx.report_ctx.iter().for_each(|report| {
                            ReportPrinter::new(&mut code_buffer)
                                .print::<DefaultPrintBackend>(&compiler_ctx.src_map, report)
                                .unwrap()
                        });
                    }
                }

                println!("{code_buffer}")
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

fn compile_code(code: &str) -> Result<frostbite_bytecode::Module, CompilerContext>
{
    let mut compiler = Compiler::new();

    let src_id = compiler.add_source("REPL".to_string(), code);

    compiler
        .compile_module(src_id, CodegenBackends::bytecode_backend())
        .map(|CompilationResults { codegen_output }| codegen_output)
        .map_err(|_| (compiler.move_ctx()))
}
