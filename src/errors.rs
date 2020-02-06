use annotate_snippets::{
    display_list::DisplayList, formatter::DisplayListFormatter, snippet::Snippet,
};
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
