use std::{env, fs, io::Write, path::PathBuf, process};

use color_eyre::eyre;

use permafrost_compiler::{
    codegen::{CodegenBackend, CodegenOutput, PrintableCodegenOutput},
    context::CompilerContext,
    utils::CompilationResult,
    Compiler, CompilerError, PERMAFROST_FILE_EXTENSION,
};

use permafrost_reports::{
    printer::{DefaultPrintBackend, ReportPrinter},
    sourcemap::{SourceKey, SourceUrl},
};

use crate::cli::CodegenKind;

pub fn compile_file_entry(
    file_path: PathBuf,
    output_file: Option<PathBuf>,
    disassemble: bool,
    codegen_kind: CodegenKind,
) -> eyre::Result<()>
{
    match file_path
        .extension()
        .map(|extension| extension.to_string_lossy())
        .as_deref()
    {
        Some(extension) if extension == PERMAFROST_FILE_EXTENSION => (),
        Some(..) | None => log::warn!(
            "The recommended file extension is `{}`",
            PERMAFROST_FILE_EXTENSION
        ),
    }

    let source_code = fs::read_to_string(&file_path)?;

    let codegen = codegen_kind.into_codegen_backend();

    let mut ctx = CompilerContext::new();

    let file_codegen_output = compile_file(&mut ctx, source_code, file_path, codegen)
        .map_err(|_| bail(&ctx))
        .unwrap();

    if disassemble {
        if let Some(printable) = file_codegen_output.as_printable() {
            disassemble_and_print(printable)?;
        } else {
            log::warn!("Codegen output doesnt support printing");
        }
    }

    write_output_to_file(&file_codegen_output, output_file)?;

    Ok(())
}

fn bail(context: &CompilerContext) -> !
{
    print_reports(context);

    process::exit(1);
}

fn print_reports(ctx: &CompilerContext)
{
    let mut buf = String::new();

    let mut report_printer = ReportPrinter::new(&mut buf);

    report_printer
        .print_reports::<DefaultPrintBackend, _>(&ctx.src_map, &*ctx.report_ctx)
        .unwrap();

    println!("{buf}");
}

fn disassemble_and_print<C>(output: &C) -> eyre::Result<()>
where
    C: PrintableCodegenOutput + ?Sized,
{
    let mut buf = String::new();

    output.print(&mut buf)?;

    println!("{buf}");

    Ok(())
}

fn write_output_to_file(
    output: &impl CodegenOutput,
    output_file: Option<PathBuf>,
) -> eyre::Result<()>
{
    let mut buf = vec![];

    output.serialize(&mut buf);

    let output_file =
        output_file.unwrap_or_else(|| env::temp_dir().join("permafrost-compiler-output"));

    let mut fs_writer = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(output_file)?;

    fs_writer.write_all(&buf)?;

    Ok(())
}

pub fn compile_file<C>(
    context: &mut CompilerContext,
    src_code: impl Into<String>,
    src_url: impl Into<SourceUrl>,
    mut codegen: C,
) -> Result<C::Output, ()>
where
    C: CodegenBackend,
{
    let mut compiler = Compiler::new(context);

    let Ok((file_src_key, mut codegen_output)) =
        call_compiler(&mut compiler, src_url, src_code, &mut codegen)
    else {
        return Err(());
    };

    let file_codegen_output = codegen_output.retrieve(file_src_key).unwrap();

    Ok(file_codegen_output)
}

pub fn call_compiler<C>(
    compiler: &mut Compiler<'_>,
    src_url: impl Into<SourceUrl>,
    src_code: impl Into<String>,
    codegen: &mut C,
) -> Result<(SourceKey, CompilationResult<C>), CompilerError>
where
    C: CodegenBackend,
{
    let source_key = compiler.add_source(src_url, src_code)?;

    let compilation_results = compiler.compile(codegen)?;

    Ok((source_key, compilation_results))
}
