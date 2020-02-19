use crate::lexer::Span;
use crate::source_file::FileMap;
use annotate_snippets::{
    display_list::DisplayList,
    formatter::DisplayListFormatter,
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use std::cmp;
use std::io;
use std::io::Write;

#[derive(Default)]
pub struct ErrorCx {
    errors: Vec<Snippet>,
}

impl ErrorCx {
    pub fn print_errors(self) {
        let formatter = DisplayListFormatter::new(true, false);

        let stderr = io::stderr();
        let mut handle = stderr.lock();

        self.errors.into_iter().for_each(|snippet| {
            let dl = DisplayList::from(snippet);
            writeln!(handle, "{}", formatter.format(&dl)).expect("Failed to write to stderr!")
        });
    }

    pub fn add_error(&mut self, error: Snippet) {
        self.errors.push(error);
    }
}

pub struct ErrText {
    pub text: String,
    pub ty: AnnotationType,
}

pub fn span_to_snippet(
    span: Span,
    srcs: &FileMap,
    title: ErrText,
    annotation: ErrText,
    footer: Option<Annotation>,
) -> Snippet {
    let footer = footer.map_or(vec![], |a| vec![a]);
    Snippet {
        title: Some(Annotation {
            label: Some(title.text),
            id: None,
            annotation_type: title.ty,
        }),
        footer,
        slices: vec![span_to_slice(span, srcs, annotation)],
    }
}

// essentially the same as span_to_snippet, but will group the two spans into
// a single slice if they're in the same file.
// this can be generalized to N snippets later if needed
pub fn two_spans_to_snippet(
    span1: Span,
    span2: Span,
    srcs: &FileMap,
    title: ErrText,
    annotation1: ErrText,
    annotation2: ErrText,
    footer: Option<Annotation>,
) -> Snippet {
    let slices = if span1.file == span2.file {
        let src = srcs.get_file(span1.file);
        // if they're in the same file, the snippet source should encompass both
        // spans.
        let src_start = cmp::min(span1.start, span2.start);
        let src_end = cmp::max(span1.end, span2.end);
        let (line_start, source) = src.surrounding_lines(src_start, src_end);
        let source_start = src.lines.offset_at_line_number(line_start);

        let range1 = span_to_range(span1, source_start, &source);
        let range2 = span_to_range(span2, source_start, &source);
        vec![Slice {
            source,
            line_start,
            origin: Some(src.path.clone()),
            fold: false,
            annotations: vec![
                SourceAnnotation {
                    label: annotation1.text,
                    annotation_type: annotation1.ty,
                    range: range1,
                },
                SourceAnnotation {
                    label: annotation2.text,
                    annotation_type: annotation2.ty,
                    range: range2,
                },
            ],
        }]
    } else {
        vec![
            span_to_slice(span1, srcs, annotation1),
            span_to_slice(span2, srcs, annotation2),
        ]
    };

    let footer = footer.map_or(vec![], |a| vec![a]);
    Snippet {
        title: Some(Annotation {
            label: Some(title.text),
            id: None,
            annotation_type: title.ty,
        }),
        footer,
        slices,
    }
}

fn span_to_slice(span: Span, srcs: &FileMap, annotation: ErrText) -> Slice {
    let src = srcs.get_file(span.file);
    let (line_start, source) = src.surrounding_lines(span.start, span.end);
    let source_start = src.lines.offset_at_line_number(line_start);

    let range = span_to_range(span, source_start, &source);
    Slice {
        source,
        // the source_file algos are 0 indexed, but for display we want lines to be 1 indexed
        line_start: line_start + 1,
        origin: Some(src.path.clone()),
        fold: false,
        annotations: vec![SourceAnnotation {
            label: annotation.text,
            annotation_type: annotation.ty,
            range,
        }],
    }
}

fn span_to_range(span: Span, slice_start: usize, source: &String) -> (usize, usize) {
    // convert absolute offsets into offsets relative to the start of `source`
    let mut start = span.start - slice_start;
    let mut end = span.end - slice_start;

    // for some reason annotate_snippets thinks \n is two chars instead of one,
    // so we need to update the offsets to compensate by adding +1 for each \n
    start += source[..start].chars().filter(|c| c == &'\n').count();
    end += source[..end].chars().filter(|c| c == &'\n').count();
    (start, end)
}
