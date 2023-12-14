use std::fmt::Display;

use frostbite_parser::{ast::Program, lexer::tokenize, Parser};
use frostbite_reports::{
    print::ReportPrinter, print_backend::DefaultPrintBackend, IntoReport, Report,
};

pub fn lex_and_parse(content: &str) -> Result<Program<'_>, Vec<Report>> {
    let token_stream = tokenize(content).map_err(|err| {
        err.into_iter()
            .map(|err| err.into_report(()))
            .collect::<Vec<_>>()
    })?;

    let parser = Parser::with_tokenstream(token_stream);

    parser.parse().map_err(|errors| {
        errors
            .into_iter()
            .map(|error| error.into_report(()))
            .collect::<Vec<_>>()
    })
}

pub fn print_report(source_id: Option<impl Display + ToString>, source: &str, report: &Report) {
    let mut buf = String::new();

    ReportPrinter::new(&mut buf)
        .print::<DefaultPrintBackend>(source_id, source, report)
        .unwrap();

    println!("{buf}")
}
