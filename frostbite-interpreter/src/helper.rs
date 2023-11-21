use frostbite_report_interface::Location;

pub enum SourceKind<'input> {
    Span(&'input str),
    Line(&'input str),
}

pub fn get_source_from_location(source: &str, location: Location) -> SourceKind<'_> {
    match location {
        // Return the whole line, since we have only a starting point in the line
        Location::Location(location) => {
            let mut line_number = 1;

            for (index, char) in source.char_indices() {
                if char == '\n' {
                    line_number += 1;
                } else if index == location {
                    break;
                }
            }

            SourceKind::Line(source.lines().nth(line_number).unwrap())
        }
        // Return the span,
        Location::Span(span) => SourceKind::Span(&source[span]),
    }
}
