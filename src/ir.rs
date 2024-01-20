// // An intermediate representation coming out of "typer", which could be optimised
// // and should allow efficient interpretation.

// // TODO - Should we include types here?

use crate::{typer::TypedExpr, ast::BinaryOperator};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Value {
  Bool(bool),
  Int(i64),
  Double(f64),
  String(String),
  Nil,
}

#[derive(Debug, PartialEq)]
pub enum IR {
  // Lookup a value from the environment.
  // Uses a symbol/reference that could be `.` separated.
  Lookup(String),
  // Apply a built-in function, evaluate the IR for each argument.
  FunctionApply(String, Vec<IR>),
  // Create a list from evaluating a bunch of IR.
  MakeList(Vec<IR>),
  Merge(Box<IR>, Box<IR>),
  // A literal to return.
  Literal(Value),
  // A structure literal
  StructLiteral(HashMap<String, IR>),
}

// We override field X with value from IR
#[derive(Debug, PartialEq)]
pub struct FieldOverride(String, Box<IR>);

// // Convert from our language to something closer to existing OTTL.
pub fn from_typed_ast(expr: TypedExpr) -> IR {
    match expr {
        TypedExpr::List(args, _) =>
          IR::MakeList(args.into_iter().map(from_typed_ast).collect()),
        TypedExpr::BinaryOp(l, BinaryOperator::Equals, r, _) => 
           IR::FunctionApply("Equals".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::NotEquals, r, _) => 
           IR::FunctionApply("NotEquals".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::In, r, _) => 
          IR::FunctionApply("Contains".into(), vec!(from_typed_ast(*r), from_typed_ast(*l))),
          TypedExpr::BinaryOp(l, BinaryOperator::Plus, r, _) => 
          IR::FunctionApply("Plus".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::Minus, r, _) => 
          IR::FunctionApply("Minus".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::Times, r, _) => 
          IR::FunctionApply("Times".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::Divide, r, _) => 
          IR::FunctionApply("Divide".into(), vec!(from_typed_ast(*l), from_typed_ast(*r))),
        TypedExpr::BinaryOp(l, BinaryOperator::With, r, _) => 
          IR::Merge(Box::new(from_typed_ast(*l)), Box::new(from_typed_ast(*r))),
        TypedExpr::Application(func, args, _) =>
          // We need to flatten the expr to a lookup.
          match from_typed_ast(*func) {
            IR::Lookup(name) => IR::FunctionApply(name, args.into_iter().map(from_typed_ast).collect()),
            _ => panic!("Cannot call function from type!")
          },
        TypedExpr::Accessor(expr, name, _) =>
          // We need to flatten the expr to a lookup
          match from_typed_ast(*expr) {
            IR::Lookup(first) => IR::Lookup(format!("{}.{}", first, name)),
            _ => panic!("Cannot access member on non-lookup"),
          },
        TypedExpr::Id(name, _) => IR::Lookup(name),
        TypedExpr::Bool(value) => IR::Literal(Value::Bool(value)),
        TypedExpr::Int(value) => IR::Literal(Value::Int(value)),
        TypedExpr::String(value) => IR::Literal(Value::String(value)),
        TypedExpr::Nil => IR::Literal(Value::Nil),
        TypedExpr::StructureConstruction(fields, _) => {
          IR::StructLiteral(
            fields.into_iter()
            .map(|s| {
              (s.field_name().into(), from_typed_ast(s.field_value()))
            })
            .collect())
        },
    }
}

impl std::fmt::Display for IR {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        IR::Lookup(id) => write!(f, "{}", id),
        IR::FunctionApply(name, args) => {
          write!(f, "{}(", name)?;
          let mut first = true;
          for arg in args {
            if !first {
              write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
            first = false;
          }
          write!(f, ")")
        },
        IR::MakeList(args) => {
          write!(f, "[")?;
          let mut first = true;
          for arg in args {
            if !first {
              write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
            first = false;
          }
          write!(f, "]")
        },
        IR::Merge(lhs, rhs) => write!(f, "Merge({},{}", lhs, rhs),
        IR::Literal(value) => write!(f, "{}", value),
        IR::StructLiteral(fields) => {
          write!(f, "{{")?;
          let mut first = true;
          for (field, value) in fields {
            if !first {
              write!(f, ", ")?;
            }
            write!(f, "{}: {}", field, value)?;
            first = false;
          }
          write!(f, "}}")
        },
    }
  }
}

impl std::fmt::Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        Value::Bool(v) => write!(f, "{}", v),
        Value::Int(v) => write!(f, "{}", v),
        Value::Double(v) => write!(f, "{}", v),
        Value::String(v) => write!(f, "\"{}\"", v), // TODO - esacpe the string
        Value::Nil => write!(f, "Nil"),
    }
  }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;
    use crate::typer::{SymbolTable, Typer, TypeError, Type};
    use crate::parser::{parse_expr, mk_parser_input};

    fn parse_to_ir(input: &str, typer: &mut Typer) -> Result<IR, Box<dyn Error>> {
        let pinput = mk_parser_input(input);
        match parse_expr(pinput) {
            Ok((rest, expr)) => {
                assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
                let texpr = typer.type_expr(expr)?;
                let ir = from_typed_ast(texpr);
                Ok(ir)
            },
            // TODO - Figure out error handling in a better way.
            Err(_) => Err(Box::new(TypeError::todo())),
        }
    }

    fn parse_to_ir_std_env(input: &str) -> Result<IR, Box<dyn Error>> {
        // TODO - Register standard environment variables.
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Int());
        symbols.register_simple_term("y", Type::Int());
        let mut typer = Typer::new(symbols);
        parse_to_ir(input, &mut typer)
    }


    #[test]
    fn converts_simple_addition() {
        let result = parse_to_ir_std_env("x + y").unwrap();
        assert_eq!(result, IR::FunctionApply("Plus".into(),
          vec!(IR::Lookup("x".into()), IR::Lookup("y".into()))
        ));
    }

    #[test]
    fn test_structure_construction_and_merge() {
      let mut symbols = SymbolTable::new();
      symbols.register_simple_term("self", Type::Simple("Span"));
      let mut typer = Typer::new(symbols);
      let result = parse_to_ir( "self with { status: { code: nil } }", &mut typer).unwrap();
      assert_eq!(result, IR::Merge(
        Box::new(IR::Lookup("self".into())),
        Box::new(IR::StructLiteral(HashMap::from([
            ("status".into(), IR::StructLiteral(HashMap::from([
              ("code".into(), IR::Literal(Value::Nil)),
            ])))
          ])
        ))
      ))
  }
}