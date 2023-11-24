use core::ops::Range;

use crate::Location;

pub fn get_line_from_location(source: &str, location: usize) -> usize {
    let mut line_number = 1;

    for (index, char) in source.char_indices() {
        if char == '\n' {
            line_number += 1;
        } else if index == location {
            break;
        }
    }

    line_number
}
