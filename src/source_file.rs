use std::cmp;
use std::iter;

use crate::span::{FileId, Location};

pub struct FileMap(pub Vec<SourceFile>);

impl FileMap {
    pub fn new() -> FileMap {
        FileMap(Vec::new())
    }

    pub fn add_file(&mut self, path: String, contents: String) -> &SourceFile {
        let id = FileId(self.0.len());
        self.0.push(SourceFile::new(id, path, contents));
        self.get_file(id)
    }

    pub fn get_file(&self, id: FileId) -> &SourceFile {
        &self.0[id.0]
    }
}

pub struct SourceFile {
    pub id: FileId,
    pub path: String,
    pub contents: String,
    pub lines: Lines,
}

impl SourceFile {
    pub fn new(id: FileId, path: String, contents: String) -> SourceFile {
        let lines = Lines::new(&contents);
        SourceFile {
            id,
            path,
            contents,
            lines,
        }
    }

    pub fn contents(&self) -> &str {
        &self.contents
    }

    pub fn surrounding_lines(&self, start: usize, end: usize) -> (usize, String) {
        let (start_line, end_line) = self.lines.surrounding_lines(start, end);
        let start = self.lines.offset_at_line_number(start_line);
        let end = self.lines.offset_at_line_end(end_line);
        (start_line, self.contents[start..end].to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Lines {
    pub line_offsets: Vec<usize>,
    pub end: usize,
}

impl Lines {
    pub fn new(data: &str) -> Lines {
        let input_indices = data
            .as_bytes()
            .iter()
            .enumerate()
            .filter(|&(_, b)| *b == b'\n')
            .map(|(i, _)| i + 1);

        let line_offsets = iter::once(0).chain(input_indices).collect();
        Lines {
            line_offsets,
            end: data.len(),
        }
    }

    /// Return the byte offset of `line_number`
    pub fn offset_at_line_number(&self, line_number: usize) -> usize {
        assert!(
            line_number <= self.max_line_num(),
            "tried to access oob line"
        );
        self.line_offsets[line_number]
    }

    pub fn offset_at_line_end(&self, line_number: usize) -> usize {
        assert!(
            line_number <= self.max_line_num(),
            "tried to access oob line"
        );
        self.line_offsets
            .get(line_number + 1)
            .map(|n| *n)
            .unwrap_or(self.end)
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

            self.line_offsets
                .get(line_index)
                .map(|line_offset| Location {
                    line: line_index,
                    col: offset - line_offset,
                    absolute: offset,
                })
        } else {
            None
        }
    }

    pub fn surrounding_lines(&self, start: usize, end: usize) -> (usize, usize) {
        (
            self.line_number_at_offset(start).saturating_sub(2),
            cmp::min(self.line_number_at_offset(end) + 2, self.max_line_num()),
        )
    }

    pub fn max_line_num(&self) -> usize {
        self.line_offsets.len() - 1
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_to_lines() {
        let lines = Lines::new("0123\n56\n89abc\nef");
        assert_eq!(lines.line_offsets, vec![0, 5, 8, 14]);
        assert_eq!(lines.end, 16);
    }

    #[test]
    fn surrounding_lines() {
        //                      0      6       13     19      26     32
        let lines = Lines::new("first\nsecond\nthird\nfourth\nfifth\nsixth");
        assert_eq!(lines.line_offsets, vec![0, 6, 13, 19, 26, 32]);

        assert_eq!(lines.line_number_at_offset(0), 0);
        assert_eq!(lines.line_number_at_offset(2), 0);
        assert_eq!(lines.surrounding_lines(0, 2), (0, 2));

        assert_eq!(lines.line_number_at_offset(32), 5);
        assert_eq!(lines.line_number_at_offset(34), 5);
        assert_eq!(lines.surrounding_lines(32, 34), (3, 5));

        assert_eq!(lines.surrounding_lines(0, 15), (0, 4));
        assert_eq!(lines.surrounding_lines(13, 14), (0, 4));
        assert_eq!(lines.surrounding_lines(13, 20), (0, 5));
        assert_eq!(lines.surrounding_lines(13, 20), (0, 5));
        assert_eq!(lines.surrounding_lines(1, 34), (0, 5));
    }

    #[test]
    fn offset_to_line() {
        //                      0      6       13     19      26     32
        let lines = Lines::new("first\nsecond\nthird\nfourth\nfifth\nsixth");
        assert_eq!(lines.line_offsets, vec![0, 6, 13, 19, 26, 32]);

        assert_eq!(lines.offset_at_line_number(0), 0);
        assert_eq!(lines.offset_at_line_end(0), 6);

        assert_eq!(lines.offset_at_line_number(2), 13);
        assert_eq!(lines.offset_at_line_end(2), 19);

        assert_eq!(lines.offset_at_line_number(5), 32);
        assert_eq!(lines.offset_at_line_end(5), 37);
    }
}
