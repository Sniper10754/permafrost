use frostbite_parser::{ast::Program, lexer::tokenize, Parser};
use frostbite_reports::{
    diagnostic_printer::{DefaultPrintBackend, DiagnosticPrinter},
    sourcemap::{SourceId, SourceMap},
    IntoReport, Report,
};

pub fn lex_and_parse<'ast, 'id>(
    content: &'ast str,
    source_id: SourceId<'id>,
) -> Result<Program<'ast>, Vec<Report<'id>>> {
    let token_stream = tokenize(content).map_err(|err| {
        err.into_iter()
            .map(|err| err.into_report(()))
            .collect::<Vec<_>>()
    })?;

    let parser = Parser::with_tokenstream(token_stream, source_id);

    parser.parse().map_err(|errors| {
        errors
            .into_iter()
            .map(|error| error.into_report(()))
            .collect::<Vec<_>>()
    })
}

pub fn print_report(
    report_source_id: SourceId<'_>,
    sources: &SourceMap<'_, '_>,
    report: &Report<'_>,
) {
    let mut buf = String::new();

    match report {
        Report::Diagnostic(diagnostic) => {
            DiagnosticPrinter::new(&mut buf)
                .print::<DefaultPrintBackend>(report_source_id, sources, diagnostic)
                .unwrap();
        }
        Report::Backtrace(_) => todo!(),
    }

    println!("{buf}")
}
