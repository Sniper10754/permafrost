use frostbite_report_interface::Location;

pub enum SourceKind<'input> {
    Span(&'input str),
    Line(&'input str),
}

pub fn get_source_from_location(source: &str, location: Location) -> SourceKind<'_> {
    
}