use argh::FromArgs;
use std::env;
use std::fs::File;
use std::io::Read;
// use walkdir::WalkDir;

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

lalrpop_mod!(pub grammar);
pub mod lexer;
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
    let file = File::open(&args.file);
    assert!(file.is_ok(), "Could not open file: {}", args.file);

    let mut contents = String::new();
    let read_result = file.unwrap().read_to_string(&mut contents);
    assert!(read_result.is_ok(), "Could not read file: {}", args.file);

    contents = contents.replace('\r', "");
    let fidl = grammar::FileParser::new().parse(lexer::Lexer::new(&contents));
    if fidl.is_err() {
        println!("Parsing failed!");
    } else {
        println!("Parsed successfully!");
    }
}
