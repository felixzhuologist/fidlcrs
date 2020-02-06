use argh::FromArgs;
use std::fs::File;
use std::io::Read;
// use walkdir::WalkDir;

#[macro_use]
extern crate lalrpop_util;
// #[macro_use]
// extern crate lazy_static;

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
    /// specify a FIDL file
    file: String,
}

fn main() {
    let args: Args = argh::from_env();
    let path = args.file;
    let file = File::open(&path);
    assert!(file.is_ok(), "Could not open file: {}", path);

    let mut contents = String::new();
    let read_result = file.unwrap().read_to_string(&mut contents);
    assert!(read_result.is_ok(), "Could not read file: {}", path);

    contents = contents.replace('\r', "");

    let mut lib_cx = source_file::FileMap::new();
    let mut error_cx = errors::ErrorCx::default();
    {
        let src_file = lib_cx.add_file(path, contents);
        match parser::parse(src_file) {
            Ok(_) => println!("Parsed successfully!"),
            Err(err) => {
                error_cx.add_error(err.into_snippet(&lib_cx));
                println!("Parsing failed");
            }
        };
    }
    error_cx.print_errors();
}
