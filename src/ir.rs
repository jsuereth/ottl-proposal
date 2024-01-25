// // An intermediate representation coming out of "typer", which could be optimised
// // and should allow efficient interpretation.

// // TODO - Should we include types here?

use crate::{typer::{TypedExpr, TypedStatement, TypedStatementType, TypedPatternMatch, TypedExtractor}, ast::BinaryOperator, ast::StreamIdentifier, transform};
use std::collections::{HashMap, BTreeMap};

#[derive(Clone, Debug, PartialEq)]
pub struct IRStatement {
  // Identifier denoting the type of stream this applies to.
  pub stream_id: StreamIdentifier,
  // An expression returning true or false determining if
  // this statement should execute on an input.
  pub guard: IR,
  // What to do with the input.
  pub expr: IR,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Bool(bool),
  Int(i64),
  Double(f64),
  String(String),
  Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IR {
  // Lookup a value from the environment.
  // Uses a symbol/reference that could be `.` separated.
  Lookup(String),
  // Apply a built-in function, evaluate the IR for each argument.
  FunctionApply(String, Vec<IR>),
  // Create a list from evaluating a bunch of IR.
  MakeList(Vec<IR>),
  // A merge operation.
  // This should be erased in one of the transformation stages.
  Merge(Box<IR>, Box<IR>),
  // A literal to return.
  Literal(Value),
  // A structure literal
  // This should be erased by the time we're done transforming.
  StructLiteral(HashMap<String, IR>),
  // A set of expressions to evaluate in parallel.
  // These are created when erasing `StructLiteral` and `Merge`.
  MultiExpr(Vec<IR>),
}

// We override field X with value from IR
#[derive(Debug, PartialEq)]
pub struct FieldOverride(String, Box<IR>);

// Returns an IR "guard" (boolean expression) and list of name->expr overrides for other IR.
fn pattern_replacements(expr: IR, pattern: TypedExtractor) -> (Option<IR>, BTreeMap<String, IR>) {
  match pattern {
    TypedExtractor::Term(id, _) => (None, [(id, expr)].into()),
    TypedExtractor::Pattern(name, args, _) => {
      let extractor = IR::FunctionApply(name, vec![expr]);
      let guard = IR::FunctionApply("IsSome".into(), vec!(extractor.clone()));
      let ex_opt = IR::FunctionApply("OptGet".into(), vec!(extractor.clone()));
      let is_tuple = args.len() > 1;
      args.into_iter()
      .enumerate()
      .map(|(idx, arg)| {        
        let arg_access_expr =
          if is_tuple {
            IR::FunctionApply(format!("TupleGet{idx}"), vec!(ex_opt.clone()))
          } else {
            ex_opt.clone()
          };
        pattern_replacements(arg_access_expr, arg)
      }).fold((Some(guard), BTreeMap::new()), |(guard, patterns), (next_guard, next_patterns)| {
        let new_guard = 
          match (guard, next_guard) {
            (Some(lhs), Some(rhs)) => Some(IR::FunctionApply("and".into(), vec!(lhs, rhs))),
            (g @ Some(_), None) | (None, g @ Some(_)) => g,
            (None, None) => None,
          };
        let mut new_patterns = BTreeMap::new();
        new_patterns.extend(patterns.into_iter());
        new_patterns.extend(next_patterns.into_iter());
        (new_guard, new_patterns)
      })
    },
  }
}

// Replaces lookups with their pattern matching expression
fn replace_raw(ir: IR, replacements: &BTreeMap<String, IR>) -> IR {
  match &ir {
    IR::Lookup(name) => 
      match replacements.get(name) {
        Some(v) => v.clone(),
        None => ir,
      },
    _ => ir,
  }
}

// Returns the guard expression *and* pattern match name replacements.
fn pattern_match_replacements(pattern: TypedPatternMatch) -> (IR, BTreeMap<String, IR>) {
  let TypedPatternMatch(expr, pattern, guard) = pattern;
  let base_expr = from_typed_ast(*expr);
  let (pguard, replacements) = pattern_replacements(base_expr, pattern);
  // Replace guard's expressions with replacements
  // from pattern matching.
  let final_guard = match (guard, pguard) {
    (Some(expr), Some(p)) => {
      let gir = from_typed_ast(expr);
      let nguard = crate::transform::transform_helper(gir, |ir| replace_raw(ir, &replacements));
      IR::FunctionApply("And".into(), vec!(p, nguard))
    },
    (Some(p), None) => crate::transform::transform_helper(
      from_typed_ast(p), |ir| replace_raw(ir, &replacements)),
    (None, Some(p)) => p,
    (None, None) => IR::Literal(Value::Bool(true)),
  };  
  (final_guard, replacements)
}

pub fn from_typed_statement(stmt: TypedStatement) -> IRStatement {
  match (stmt.opt_pattern(), stmt.where_expr()) {
    (Some(pattern), Some(guard)) => {
      //  We need to unify the guard w/ the guard form the pattern.
      todo!()
    },
    (Some(pattern), None) => {
      let (guard, replacements) = pattern_match_replacements(pattern.clone());
      IRStatement {
        stream_id: stmt.stream_id(),
        guard,
        expr: match stmt.statement_type() {
          TypedStatementType::Drop => IR::FunctionApply("Drop".into(), [].into()),
          TypedStatementType::Yield(expr) => {
            let raw = from_typed_ast(*expr.clone());
            transform::transform_helper(raw, |ir| replace_raw(ir, &replacements))
          },
        }
      }
    },
    (None, Some(guard)) => {
      IRStatement {
        stream_id: stmt.stream_id(),
        guard: from_typed_ast(guard.clone()),
        expr: match stmt.statement_type() {
          TypedStatementType::Drop => IR::FunctionApply("Drop".into(), [].into()),
          TypedStatementType::Yield(expr) => from_typed_ast(*expr.clone()),
        },
      }
    },
    (None, None) => {
      IRStatement {
        stream_id: stmt.stream_id(),
        guard: IR::Literal(Value::Bool(true)),
        expr: match stmt.statement_type() {
          TypedStatementType::Drop => IR::FunctionApply("Drop".into(), [].into()),
          TypedStatementType::Yield(expr) => from_typed_ast(*expr.clone()),
        },
      }
    },
  }
}

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
        IR::MultiExpr(exprs) => {
          let mut first = true;
          for expr in exprs {
            if !first {
              write!(f, "\n")?;
            }
            write!(f, "{}", expr)?;
            first = false;
          }
          Ok(())
        }
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

impl std::fmt::Display for IRStatement {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "context: {}, guard: {}, expr: {}", self.stream_id, self.guard, self.expr)
  }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;
    use crate::typer::{SymbolTable, Typer, TypeError, Type};
    use crate::parser::{parse_expr, mk_parser_input, parse_statement};

    fn parse_to_ir_statement(input: &str) -> Result<IRStatement, Box<dyn Error>> {
      let mut typer = Typer::new(SymbolTable::new());
      let pinput = mk_parser_input(input);
      match parse_statement(pinput) {
          Ok((rest, stmt)) => {
              assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
              let tstmt = typer.type_statement(stmt)?;
              let ir = from_typed_statement(tstmt);
              Ok(ir)
          },
          // TODO - Figure out error handling in a better way.
          Err(_) => Err(Box::new(TypeError::todo())),
      }
   }

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

  #[test]
  fn test_safely_translates_pattern_match() {
    let result = parse_to_ir_statement(
      "on metric when metric is aSum(sum) yield metric with { sum: sum }"
    ).unwrap();
    assert_eq!(result.stream_id, StreamIdentifier::Metric);
    assert_eq!(format!("{}", result.guard), "IsSome(aSum(metric))");
    assert_eq!(format!("{}", result.expr), "Merge(metric,{sum: OptGet(aSum(metric))}");
  }
}