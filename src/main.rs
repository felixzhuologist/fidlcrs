use argh::FromArgs;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
// use walkdir::WalkDir;

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod errors;
lalrpop_mod!(pub grammar);
pub mod lexer;
pub mod parser;
pub mod raw;
pub mod source_file;
pub mod span;
pub mod token;

#[derive(FromArgs)]
/// The FIDL compiler, rust edition
struct Args {
    #[argh(option)]
    /// a comma separated list of fidl files corresponding to a single library
    files: String,
}

fn main() {
    let args: Args = argh::from_env();
    let filenames: Vec<String> = args.files.split(',').map(str::to_string).collect();

    let mut lib_cx = source_file::FileMap::new();
    let mut error_cx = errors::ErrorCx::default();
    let mut files: Vec<raw::File> = Vec::new();
    let mut lib_names: HashMap<String, lexer::Span> = HashMap::new();
    for path in filenames {
        let raw_contents = read_file(&path);
        let src_file = lib_cx.add_file(path, raw_contents);
        match parser::parse(src_file) {
            Ok(file) => {
                // could just .extend(.map) if this were a vector
                for error in raw::validate::validate_file(&file) {
                    error_cx.add_error(error.into_snippet(&src_file));
                }
                for decl in &file.decls {
                    lib_names.insert(decl.value.name(), decl.span);
                }
                files.push(file);
            }
            Err(err) => {
                error_cx.add_error(err.into_snippet(&src_file));
                println!("Parsing failed");
            }
        };
    }

    error_cx.print_errors();
}

fn read_file(path: &String) -> String {
    let file = File::open(path);
    assert!(file.is_ok(), "Could not open file: {}", path);

    let mut contents = String::new();
    let read_result = file.unwrap().read_to_string(&mut contents);
    assert!(read_result.is_ok(), "Could not read file: {}", path);

    contents.replace('\r', "")
}
