use argh::FromArgs;

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

pub mod errors;
pub mod flat;
pub mod flatten;
pub mod ir;
lalrpop_mod!(pub grammar);
pub mod lexer;
pub mod parser;
pub mod raw;
pub mod source_file;
pub mod span;
pub mod token;
pub mod typeshape;

use flat::{add_library, resolve_library, validate_latest_library};
use flatten::flatten_files;
use parser::parse_files;
use raw::validate_files;
use std::fs;

#[derive(FromArgs)]
/// The FIDL compiler, rust edition
struct Args {
    #[argh(option)]
    /// a comma separated list of fidl files corresponding to a single library
    files: Vec<String>,
    #[argh(option)]
    /// path to write JSON IR output
    json: Option<String>,
}

fn main() {
    let args: Args = argh::from_env();

    match compile(args.files) {
        Ok(ir) => match args.json {
            Some(path) => fs::write(path, serde_json::to_string_pretty(&ir).unwrap())
                .expect("couldn't write to  out file"),
            None => println!("no errors found!"),
        },
        Err(errs) => errs.print_errors(),
    };
}

fn compile(files: Vec<String>) -> Result<ir::Library, errors::ErrorCx> {
    let mut libs = flat::Libraries::default();
    let mut errors = errors::ErrorCx::default();
    let mut srcs = source_file::FileMap::new();

    for lib_files in files {
        let filenames: Vec<String> = lib_files.split(',').map(str::to_string).collect();

        let raw_asts = parse_files(&mut srcs, &mut errors, filenames);
        if raw_asts.is_empty() {
            return Err(errors);
        }

        validate_files(&srcs, &mut errors, &raw_asts);

        let unresolved_lib = flatten_files(&srcs, &mut errors, raw_asts);
        let resolved_lib = match resolve_library(&srcs, unresolved_lib, &libs) {
            Ok(lib) => lib,
            Err(errs) => {
                errors.extend(errs);
                return Err(errors);
            }
        };

        add_library(&mut errors, &mut libs, resolved_lib);
        validate_latest_library(&srcs, &mut errors, &libs);
    }

    if errors.is_empty() {
        Ok(libs.to_ir())
    } else {
        Err(errors)
    }
}
