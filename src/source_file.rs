use std::iter;

use crate::span::Location;

#[derive(Clone, Debug)]
pub struct Lines {
    pub line_offsets: Vec<usize>,
    pub end: usize,
}

impl Lines {
    pub fn new(line_offsets: Vec<usize>, end: usize) -> Lines {
        Lines { line_offsets, end }
    }

    /// Return the byte offset of `line_number`
    pub fn offset_at_line_number(&self, line_number: usize) -> Option<usize> {
        self.line_offsets.get(line_number).cloned()
    }

    /// Return the line number that this offset is a part of
    pub fn line_number_at_offset(&self, offset: usize) -> usize {
        let num_lines = self.line_offsets.len();

        (0..num_lines)
            .filter(|&i| self.line_offsets[i] > offset)
            // the first remaining line will be 1 past the one containing the offset
            .map(|i| i - 1)
            .next()
            .unwrap_or(num_lines - 1)
    }

    /// Convert an offset into a Location
    pub fn location(&self, offset: usize) -> Option<Location> {
        if offset <= self.end {
            let line_index = self.line_number_at_offset(offset);

            self.offset_at_line_number(line_index)
                .map(|line_offset| Location {
                    line: line_index,
                    col: offset - line_offset,
                    absolute: offset,
                })
        } else {
            None
        }
    }
}

pub struct SourceFile<'a> {
    pub data: &'a str,
    pub lines: Lines,
}

impl<'a> SourceFile<'a> {
    pub fn new(src: &str) -> SourceFile {
        SourceFile {
            data: src,
            lines: Lines::new(to_offsets(src), src.len()),
        }
    }

    pub fn data(&self) -> &'a str {
        self.data
    }
}

pub fn to_offsets(src: &str) -> Vec<usize> {
    let input_indices = src
        .as_bytes()
        .iter()
        .enumerate()
        .filter(|&(_, b)| *b == b'\n')
        .map(|(i, _)| i + 1);

    iter::once(0).chain(input_indices).collect()
}
