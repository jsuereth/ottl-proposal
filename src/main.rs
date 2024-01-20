pub mod ast;
pub mod parser;
pub mod typer;
pub mod ir;

use std::error::Error;
use crate::typer::{SymbolTable, Type, Typer, TypeError};
use crate::parser::{parse_expr, mk_parser_input};
use crate::ir::from_typed_ast;

// Compiles input into intermediary representation output.
fn transpile(input: &str) -> Result<String, Box<dyn Error>> {
    let mut symbols = SymbolTable::new();
    symbols.register_simple_term("span", Type::Simple("Span"));
    symbols.register_simple_term("metric", Type::Simple("Metric"));
    symbols.register_simple_term("resource", Type::Simple("Resource"));
    let mut typer = Typer::new(symbols);
    let pinput = mk_parser_input(input);
    match parse_expr(pinput) {
        Ok((rest, expr)) => {
            assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
            let texpr = typer.type_expr(expr)?;
            let ir = from_typed_ast(texpr);
            // TODO - Run through some optimisation phases.
            Ok(format!("{}", ir))
        },
        // TODO - Figure out error handling in a better way.
        Err(_) => Err(Box::new(TypeError::todo())),
    }
}

fn main() {

    for filename in std::env::args().skip(1) {
        println!("Transpiling {}!", filename);
        let input = std::fs::read_to_string(filename).expect("Unable to find the file!");
        let result = transpile(&input).expect("Compilation failed!");
        println!("--------");
        println!("{}", result);
        println!("--------");
    }
    
    
}
