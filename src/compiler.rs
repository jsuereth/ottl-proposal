use std::error::Error;
use crate::typer::{SymbolTable, Typer, TypeError};
use crate::parser::{parse_statement, mk_parser_input};
use crate::ir::from_typed_statement;

// Compiles input into intermediary representation output.
pub fn transpile(input: &str) -> Result<String, Box<dyn Error>> {
    // TODO - allow more built-in symbols or types.
    let mut typer = Typer::new(SymbolTable::new());
    let pinput = mk_parser_input(input);
    match parse_statement(pinput) {
        Ok((rest, stmt)) => {
            assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
            let texpr = typer.type_statement(stmt)?;
            let ir = from_typed_statement(texpr);
            let pass_one = crate::transform::evaluate_compile_time_expressions(ir);
            let pass_two = crate::transform::convert_merge_to_sets_on_yield(pass_one);
            // TODO - Run through some optimisation phases.
            Ok(format!("{}", pass_two))
        },
        // TODO - Figure out error handling in a better way.
        Err(_) => Err(Box::new(TypeError::todo())),
    }
}