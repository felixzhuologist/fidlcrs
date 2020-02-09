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
        let src_end = cmp::min(span1.end, span2.end);
        let (line_start, source) = src.surrounding_lines(src_start, src_end);
        let source_start = src.lines.offset_at_line_number(line_start);
        vec![Slice {
            source,
            line_start,
            origin: Some(src.path.clone()),
            fold: false,
            annotations: vec![
                SourceAnnotation {
                    label: annotation1.text,
                    annotation_type: annotation1.ty,
                    range: (span1.start - source_start, span1.end - source_start),
                },
                SourceAnnotation {
                    label: annotation2.text,
                    annotation_type: annotation2.ty,
                    range: (span2.start - source_start, span2.end - source_start),
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

    Slice {
        source,
        line_start,
        origin: Some(src.path.clone()),
        fold: false,
        annotations: vec![SourceAnnotation {
            label: annotation.text,
            annotation_type: annotation.ty,
            range: (span.start - source_start, span.end - source_start),
        }],
    }
}
