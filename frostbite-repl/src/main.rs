use frostbite_bytecode::text_repr::print_bytecode;
use frostbite_compiler::{codegen::CodegenBackends, CompilationResults, Compiler};
use frostbite_reports::{
    diagnostic_printer::{DefaultPrintBackend, DiagnosticPrinter},
    sourcemap::{SourceDescription, SourceMap},
    Report, ReportContext,
};
use reedline::{DefaultPrompt, DefaultPromptSegment, Reedline, Signal};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
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
                    Err((src_map, reports)) => {
                        reports.iter().for_each(|report| match report {
                            Report::Diagnostic(diagnostic) => {
                                DiagnosticPrinter::new(&mut code_buffer)
                                    .print::<DefaultPrintBackend>(&src_map, diagnostic)
                                    .unwrap();
                            }
                            Report::Backtrace(_) => unreachable!(),
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

fn compile_code(code: &str) -> Result<frostbite_bytecode::Module, (SourceMap, ReportContext)> {
    let mut report_ctx = ReportContext::default();
    let mut src_map = SourceMap::default();

    let src_id = src_map.insert(SourceDescription {
        url: "REPL".to_string().into(),
        source_code: code.into(),
    });

    Compiler::compile_source_code(
        &mut report_ctx,
        &mut src_map,
        src_id,
        CodegenBackends::bytecode_backend(),
    )
    .map(
        |CompilationResults {
             t_ast: _,
             codegen_output,
         }| codegen_output,
    )
    .map_err(|_| (src_map, report_ctx))
}
