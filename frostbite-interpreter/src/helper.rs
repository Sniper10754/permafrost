use frostbite_report_interface::Location;

pub enum SourceKind<'input> {
    Span(&'input str),
    Line(&'input str),
}

pub fn get_source_from_location(source: &str, location: Location) -> SourceKind<'_> {
    match location {
        // Return the whole line, since we have only a starting point in the line
        Location::Location(_) => {
            let mut current_line = 0;



            todo!()
        }
        // Return the span,
        Location::Span(span) => SourceKind::Span(&source[span]),
    }
}
