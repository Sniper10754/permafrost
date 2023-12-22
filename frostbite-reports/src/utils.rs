/// Function used to get the line number starting from a column in the file
#[must_use] pub fn get_line_from_location(source: &str, column: usize) -> usize {
    let mut line_number = 1;

    for (char_index, char) in source.char_indices() {
        if char == '\n' {
            line_number += 1;
        } else if char_index == column {
            break;
        }
    }

    line_number
}
