use argh::FromArgs;
use std::fs::File;
use std::io::Read;
// use walkdir::WalkDir;

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

pub mod errors;
pub mod flat;
pub mod flatten;
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

    let mut _dependencies = flat::resolve::Dependencies::default();
    let mut error_cx = errors::ErrorCx::default();
    // TODO: this will be per library
    {
        let filenames: Vec<String> = args.files.split(',').map(str::to_string).collect();

        let mut srcs = source_file::FileMap::new();
        let mut flattener = flatten::Flattener::default();
        for path in filenames {
            let raw_contents = read_file(&path);
            let src_file = srcs.add_file(path, raw_contents);
            match parser::parse(src_file) {
                Ok(file) => {
                    for error in raw::validate::validate_file(&file) {
                        error_cx.add_error(error.into_snippet(&srcs));
                    }
                    flattener.add_file(file);
                }
                Err(err) => {
                    error_cx.add_error(err.into_snippet(&src_file));
                    println!("Parsing failed");
                }
            };
        }
        if !flattener.files.is_empty() {
            let (_files, errors) = flattener.finish();
            for error in errors {
                error_cx.add_error(error.into_snippet(&srcs));
            }
            // let lib = flat::Library::from_files(files, &dependencies);
            // TODO: accumulate errors here
            // dependencies.add_library(lib);
        }
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
