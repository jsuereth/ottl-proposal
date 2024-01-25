use crate::ir::*;

// Helper method which will ensure a transformation function F is called
// recursively over the IR trees.
pub fn transform_helper<F>(ir: IR, f: F) -> IR
  where F: Copy + Fn(IR) -> IR {
    match ir {
        IR::FunctionApply(name, args) =>
        f(IR::FunctionApply(name, args.into_iter().map(|a| transform_helper(a, f)).collect())),
      IR::Merge(lhs, rhs) => f(IR::Merge(
          Box::new(transform_helper(*lhs, f)), 
          Box::new(transform_helper(*rhs, f)))),
      IR::StructLiteral(fields) =>
        f(IR::StructLiteral(fields.into_iter()
          .map(|(k,e)| (k, transform_helper(e, f)))
          .collect())),
      IR::MakeList(values) => f(IR::MakeList(
        values.into_iter()
        .map(|v| transform_helper(v, f))
        .collect()
      )),
      IR::MultiExpr(exprs) =>
        f(IR::MultiExpr(
            exprs.into_iter()
            .map(|e| transform_helper(e, f))
            .collect())),
      other => f(other),
    }

}


// If we preserved types we could do this better here...
fn evaluate_plus(lhs: Value, rhs: Value) -> Value {
    match (lhs, rhs) {        
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs+rhs),
        (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs+rhs),
        (Value::String(lhs), Value::String(rhs)) => Value::String(format!("{}{}", lhs, rhs)),
        // TODO - better error, also type system should prevent this.
        (lhs,rhs) => panic!("Unable to add incompatible values: {}, {}", lhs, rhs),
    }
}
fn evaluate_minus(lhs: Value, rhs: Value) -> Value {
    match (lhs, rhs) {        
        (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs-rhs),
        (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs-rhs),
        // TODO - better error, also type system should prevent this.
        (lhs,rhs) => panic!("Unable to subtract incompatible values: {}, {}", lhs, rhs),
    }
}

// We can add more optimisations here as we find the need.
fn evaluate_compile_time_function(name: String, args: Vec<IR>) -> IR {
    if name == "Plus" {
        match args.as_slice() {
            [IR::Literal(lhs), IR::Literal(rhs)] => IR::Literal(evaluate_plus(lhs.clone(), rhs.clone())),
            _ => IR::FunctionApply(name, args),
        }
    } else if name == "Minus" {
        match args.as_slice() {
            [IR::Literal(lhs), IR::Literal(rhs)] => IR::Literal(evaluate_minus(lhs.clone(), rhs.clone())),
            _ => IR::FunctionApply(name, args),
        }
    } else {
        IR::FunctionApply(name, args)
    }
}

pub fn evaluate_compile_time_expressions(ir: IR) -> IR {
    transform_helper(ir, move |ir| {
        match ir {
            IR::FunctionApply(name, args) => evaluate_compile_time_function(name,args),
            other => other,
        }
    })
}

fn join_name(name: &str, field: &str) -> String {
    format!("{}.{}", name, field)
}

// Recurse into structures and break apart merge operations into `Set` operations.
fn recursive_break_set(name: &str, field: &str, value: IR) -> Vec<IR> {
    match value {
        IR::StructLiteral(fields) => {
            let name = join_name(name, field);
            fields.into_iter()
            .flat_map(|(field, value)| recursive_break_set(&name, &field, value))
            .collect()
        },
        other => vec!(IR::FunctionApply("set".into(), vec!(IR::Lookup(join_name(name, field)), other))),
    }
}

fn transform_merge_to_set(lhs: IR, rhs: IR) -> IR {
    match [lhs,rhs] {
        [IR::Lookup(name), IR::StructLiteral(fields)] => {
            let exprs: Vec<IR> =
               fields.into_iter()
               .flat_map(|(field,value)| recursive_break_set(&name, &field, value))
               .collect();
            if exprs.len() == 1 {
                exprs[0].clone()
            } else {
                IR::MultiExpr(exprs)
            }
        },
        _ => panic!("Unable to optimise merge!"),
    }
}

// This should convert every "merge' operations into a list of `set` operations.
pub fn convert_merge_to_sets(ir: IR) -> IR {
    transform_helper(ir, move |ir| {
        match ir {
            IR::Merge(lhs,rhs) => transform_merge_to_set(*lhs, *rhs),
            other => other,
        }
    })
}