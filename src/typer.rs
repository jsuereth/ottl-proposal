use nom::{InputIter, AsChar};

use crate::ast::*;
use std::collections::BTreeMap;
use std::error::Error;

type Substitutions = BTreeMap<u64, Type>;


// TODO - Finish typed AST
#[derive(Debug)]
pub enum TypedExtractor {
    Term(String, Type),
    Pattern(String, Vec<TypedExtractor>, Type),
}

#[derive(Debug)]
pub struct TypedPatternMatch(Box<TypedExpr>, TypedExtractor, Option<TypedExpr>);

#[derive(Debug)]
pub enum TypedStatementType {
    Drop,
    Yield(Box<TypedExpr>),
}

impl TypedStatementType {
    pub fn stream_type(&self) -> Type {
        match self {
            Self::Drop => Type::Nil,
            Self::Yield(expr) => expr.my_type(),
        }
    }
}

#[derive(Debug)]
pub struct TypedStatement(pub StreamIdentifier, Option<TypedPatternMatch>, TypedStatementType, Option<TypedExpr>);

impl TypedStatement {
    pub fn stream_type(&self) -> Type {
        self.2.stream_type()
    }
}

#[derive(Debug)]
pub enum TypedExpr {
    Id(String, Type),
    List(Vec<TypedExpr>, Type),
    StructureConstruction(Vec<TypedFieldAssignment>, Type),
    Nil,
    Bool(bool),
    String(String),
    Int(i64),
    Accessor(Box<TypedExpr>, String, Type),
    BinaryOp(Box<TypedExpr>, BinaryOperator, Box<TypedExpr>, Type),
    Application(Box<TypedExpr>, Vec<TypedExpr>, Type),
    

}
impl TypedExpr {
    // TODO - avoid cloning types...
    pub fn my_type(&self) -> Type {
        match self  {
            TypedExpr::Id(_, tpe) => tpe.clone(),
            TypedExpr::BinaryOp(_, _, _, tpe) => tpe.clone(),
            TypedExpr::List(_, tpe) => tpe.clone(),
            TypedExpr::Accessor(_, _, tpe) => tpe.clone(),
            TypedExpr::Application(_, _, tpe) => tpe.clone(),
            TypedExpr::StructureConstruction(_, tpe) => tpe.clone(),
            TypedExpr::Nil => Type::Nil,
            TypedExpr::Bool(_) => Type::Boolean(),
            TypedExpr::Int(_) => Type::Int(),
            TypedExpr::String(_) => Type::String(),
        }
    }
}

#[derive(Debug)]
pub struct TypedFieldAssignment(String, Box<TypedExpr>);

impl TypedFieldAssignment {
    fn my_type(&self) -> Type {
        self.1.my_type()
    }
    pub fn field_name(&self) -> &str {
        &self.0
    }

    pub fn field_value(self) -> TypedExpr {
        *self.1
    }
}



#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    // Represents a type we have not discovered yet. These should not last
    // past typechecking.
    Variable(u64),
    // TODO - A type Constructor could just be a Type with type variables,
    // and we can have a separate AppliedType with "values" applied to the constructor.
    // Structure of one of these is inferred from symbol table.
    Constructor(String, Vec<Type>),
    // We have an ad-hoc structural type, likely from a structure literal.
    Structural(BTreeMap<String, Type>),
    // The bottom type.
    Nil,
    // A type that could be "one of the following"
    AnyValue,
}

impl Type {
    pub fn Simple(name: &str) -> Type {
        Type::Constructor(name.into(), vec!())
    }
    pub fn Boolean() -> Type {
        Type::Simple("Bool")
    }
    pub fn Int() -> Type {
        Type::Simple("Int")
    }
    pub fn Double() -> Type {
        Type::Simple("Double")
    }
    pub fn String() -> Type {
        Type::Simple("String")
    }
    pub fn List(elemType: Type) -> Type {
        Type::Constructor("List".into(), vec!(elemType))
    }
    pub fn Bytes() -> Type {
        Type::List(Type::Simple("Byte"))
    }

    pub fn ArrayValue() -> Type {
        Type::List(Type::AnyValue)
    }

    pub fn Function(result_type: Type, arg_types: Vec<Type>) -> Type {
        let mut types = vec!(result_type);
        types.extend(arg_types);
        Type::Constructor("Function".into(), types)
    }

    // Returns true if this can be coerced intoa  number that participates in +/-
    pub fn is_numeric(&self) -> bool {
        // TODO - Expand coverage here!
        self.is_int() || self.is_double()
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "Function",
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "List",
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "Bool",
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "Int",
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_double(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "Double",
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Type::Constructor(name, _) => name == "String",
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_bytes(&self) -> bool {
        match self {
            Type::Constructor(name, arg) => 
              name == "List" && arg.len() == 1 && arg[0] == Type::Simple("byte"),
            Type::Nil => true,
            _ => false,
        }
    }

    pub fn is_array_value(&self) -> bool {
        match self {
            Type::Constructor(name, arg) => 
              name == "List" && arg.len() == 1 && arg[0] == Type::AnyValue,
            Type::Nil => true,
            _ => false,
        }
    }


    pub fn is_any_value_compatible(&self) -> bool {
        self.is_bool() || 
        self.is_int() || 
        self.is_double() || 
        self.is_string() || 
        self.is_bytes() ||
        self.is_array_value()
        // TODO -
        // KeyValueList
    }

    pub fn structure_name(&self) -> Result<&str, TypeError> {
        match self {
            Type::Constructor(name, _)=> Ok(&name),
            // TODO - better error message.
            _ => Err(TypeError::todo()),
        }
    }

    pub fn get_function_return_type(&self) -> Result<Type, TypeError> {
        match self {
            Type::Constructor(name, args) => {
                if name == "Function" && args.len() > 0 {
                    // TODO - less cloning
                    Ok(args[0].clone())
                } else {
                    Err(TypeError::new_type_is_not_function(self))   
                }
            },
            _ => Err(TypeError::new_type_is_not_function(self))
        }
    }
}

fn write_structural_type(f: &mut std::fmt::Formatter<'_>, fields: &BTreeMap<String,Type>) -> std::fmt::Result {
    write!(f, "{{")?;
    let mut first_arg = true;
    // We want to sort-by-key for debugabiltiy.
    for (name, tpe) in fields {
        if !first_arg {
            write!(f, ", ")?;
        }
        write!(f, "{}:{}", name, tpe)?;
        first_arg = false;
    }
    write!(f, "}}")
}

fn write_constructor_type(f: &mut std::fmt::Formatter<'_>, name: &str, args: &Vec<Type>) -> std::fmt::Result {
    if name == "Function" {
        match  args.as_slice() {
            [rtpe] => write!(f, "() {}", rtpe),
            [] => panic!("Invalid function type: {:?}", args),
            [rtpe, arg, rest @ ..] => {
                write!(f, "({}", arg)?;
                for tpe in rest {
                    write!(f, ", {}", tpe)?;
                }
                write!(f, ") {}", rtpe)
            },
        }
    } else {
        // Fallthrough
        write!(f, "{}", name)?;
        if args.len() > 0 {
            write!(f, "[")?;
            let mut first_arg = true;
            for arg in args {
                if !first_arg {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
                first_arg = false;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO - Special handling for KeyValue + KeyValueList
        // Nil must be first, because it can masquerade as other types...
        if self.is_nil() {
            return write!(f, "Nil");
        } else if self.is_array_value() {
            return write!(f, "ArrayValue");
        } else if self.is_bytes() {
            return write!(f, "Bytes");
        }
        match self {
            Type::Variable(id) => write!(f, "?{}", id),
            Type::Nil => write!(f, "Nil"),
            Type::AnyValue => write!(f, "AnyValue"),
            Type::Structural(fields) => write_structural_type(f, fields),
            Type::Constructor(name, args) => write_constructor_type(f, name, args),
        }
    }
}



#[derive(Debug)]
pub struct TypeError {
    // TODO
    msg: String,
}
impl TypeError {
    // TODO - Improve these error messages
    fn new_types_not_equal(lhs: &Type, rhs: &Type) -> TypeError {
        TypeError{
            msg: format!("Types are not equal: {} != {}", lhs, rhs).into(),
        }
    }
    fn new_invalid_recursive_type(tpe: &Type) -> TypeError {
        TypeError{
            msg: format!("Invalid recursive type: {}", tpe).into(),
        }
    }

    fn new_type_is_not_function(tpe: &Type) -> TypeError {
        TypeError { 
            msg: format!("Expected function, found: {}", tpe).into(),
        }
    }

    fn new_unable_to_find_symbol(symbol: &str) -> TypeError {
        TypeError {
            msg: format!("Unable to find symbol: {}", symbol).into(),
        }
    }

    fn new_type_not_anyval(tpe: &Type) -> TypeError {
        TypeError {
            msg: format!("Expecteed AnyValue but found: {}", tpe).into(),
        }
    }

    pub fn new_with_custom(msg: &str) -> TypeError {
        TypeError {
            msg: msg.into(),
        }
    }

    pub fn todo() -> TypeError {
        TypeError{
            msg: format!("TODO - implement or error message.").into()
        }
    }
}

impl Error for TypeError {}
impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // TODO - Preserve location, maybe also print source where error occured.
        write!(f, "TypeError: {}", self.msg)
    }
}

#[derive(Clone, Debug)]
struct StructSymbol {
    name: String,
    fields: BTreeMap<String, Type>,
}
#[derive(Clone)]
pub struct SymbolTable {
    // Terms are name -> Nominal Types of that name.
    terms: BTreeMap<String, Type>,
    // TODO - figure out how to do lookup of members.
    structs: BTreeMap<String, StructSymbol>,
}
// TODO - symbol table lookups

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            terms: BTreeMap::new(),
            structs: BTreeMap::new(),
        }
    }

    fn lookup_id(&self, name: &str) -> Result<Type, TypeError> {
        match self.terms.get(name) {
            Some(tpe) => Ok(tpe.clone()),
            None => Err(TypeError::new_unable_to_find_symbol(name)),
        }
    }
    fn lookup_member(&self, typename: &str, member: &str) -> Result<Type, TypeError> {
        match self.structs.get(typename) {
            Some(symbol) =>
              match symbol.fields.get(member) {
                Some(tpe) => Ok(tpe.clone()),
                None => Err(TypeError::new_unable_to_find_symbol(&format!("{} on {}", member, typename))),
              },              
            None => Err(TypeError::new_unable_to_find_symbol(&format!("{} on {}", member, typename))),
        }
    }


    // Constructing a symbol table.
    pub fn register_simple_term(&mut self, name: &str, tpe: Type) {
        self.terms.insert(name.into(), tpe);
    }
    // Constructing structures
    fn register_structure(&mut self, symbol: StructSymbol) {
        self.structs.insert(symbol.name.clone(), symbol);
    }
}

// A global-inferring type system.
pub struct Typer {
    scope: SymbolTable,
    substitutions: Substitutions,
    current_id: u64,
}

impl Typer {

    pub fn new(scope: SymbolTable) -> Typer {
        let mut result = Typer {
            scope,
            substitutions: BTreeMap::new(),
            current_id: 0,
        };
        register_standard_types(&mut result);
        result
    }

    pub fn scope_mut(&mut self) -> &mut SymbolTable {
        &mut self.scope
    }

    // TODO - figure out if we can attach ownership of types to TyperState or something longer.
    fn prune(&self, tpe: Type) -> Type {
        match tpe {
            Type::Variable(id) => 
              match self.substitutions.get(&id) {
                // TODO - avoid cloning here.
                Some(result) => self.prune(result.clone()),
                None => tpe,
              },
            Type::Constructor(name, args) =>
              Type::Constructor(name, args.into_iter().map(|t| self.prune(t)).collect()),
            Type::Structural(fields) =>
              Type::Structural(
                fields.into_iter()
                .map(|(f,t)| (f, self.prune(t)))
                .collect()
              ),
            Type::AnyValue => Type::AnyValue,
            Type::Nil => Type::Nil,
        }
    }

    fn new_variable(&mut self) -> Type {
        self.current_id += 1;
        Type::Variable(self.current_id)
    }

    // This method is called when two types occupy the "same space" in the AST and must
    // conform.
    // We need to unify the two types to ensure things are allowed.
    fn unify(&mut self, lhs: Type, rhs: Type) -> Result<Type, TypeError> {
        match (self.prune(lhs), self.prune(rhs)) {
            // If we have two variables, make them equal each other.
            (Type::Variable(l), Type::Variable(r)) =>
              if l < r {
                self.substitutions.insert(l, Type::Variable(r));
                Ok(Type::Variable(r))
              } else {
                self.substitutions.insert(r, Type::Variable(l));
                Ok(Type::Variable(l))
              },
            // If we have  a variable + another type, assign the variable to the type.
            (Type::Variable(id), other) => {
                if occurs_in(id, &other) {
                    Err(TypeError::new_invalid_recursive_type(&other))
                } else {
                    self.substitutions.insert(id, other.clone());
                    Ok(other)
                }
            },
            (other, Type::Variable(id)) => {
                if occurs_in(id, &other) {
                    Err(TypeError::new_invalid_recursive_type(&other))
                } else {
                    self.substitutions.insert(id, other.clone());
                    Ok(other)
                }
            },
            // We allow nil to be converted down to any other type.
            (Type::Nil, other) => Ok(other),
            (other, Type::Nil) => Ok(other),
            // We force the two types to be equal.
            (Type::Constructor(lname, largs), Type::Constructor(rname, rargs)) => {
                // Fundamental typecheck ->
                //   Ensure name is the same, args are same length and unify each arg.
                if lname != rname || largs.len() != rargs.len() {
                    // TODO - Better Error messages
                    Err(TypeError::new_types_not_equal(&Type::Constructor(lname, largs), &Type::Constructor(rname, rargs)))
                } else {
                    let arg_result: Result<Vec<Type>, TypeError> =
                        largs.into_iter().zip(rargs.into_iter())
                        .map(|(l,r)| self.unify(l,r))
                        .collect();
                    arg_result.map(|args| Type::Constructor(lname, args))
                }
            },
            (Type::AnyValue, Type::AnyValue) => {
                Ok(Type::AnyValue)
            },
            (Type::AnyValue, other) | (other, Type::AnyValue) => {
                if other.is_any_value_compatible() {
                    Ok(Type::AnyValue)
                } else {
                    // TODO - Good error message.
                    Err(TypeError::new_type_not_anyval(&other))
                }
            },
            (Type::Structural(lfields), Type::Structural(rfields)) => self.unify_structural_fields(lfields,rfields),            

            // Handle structural types.
            // TODO - the structural type must EXACTLY match the constructor?
            (tc @ Type::Constructor(_,_), s @ Type::Structural(_)) => Err(TypeError::new_types_not_equal(&tc, &s)),
            (s @ Type::Structural(_), tc @ Type::Constructor(_, _)) => Err(TypeError::new_types_not_equal(&tc, &s)),
        }
    }

    // Checks if lhs is a subtype of rhs. This will also
    // propagate type inference from LHS to RHS if RHS is unknown.
    fn unify_subtype(&mut self, lhs: Type, rhs: Type)-> Result<Type, TypeError> {
        match (lhs, rhs) {
            (lhs @ _, rhs @ Type::Variable(_)) => self.unify(lhs, rhs),
            (other, Type::Nil) => Ok(other),
            (Type::AnyValue, other) if other.is_any_value_compatible() => Ok(Type::AnyValue),
            (Type::AnyValue, other) => Err(TypeError::new_type_not_anyval(&other)),
            // We allow structural types with known fields ot unify with real types.
            (tc @ Type::Constructor(_,_), Type::Structural(rfields)) =>
              self.unify_concrete_to_struct_subtype(tc, &rfields),
            (lhs @ Type::Constructor(_,_), rhs @ Type::Constructor(_,_)) => self.unify(lhs, rhs),
            // We can unify with the subset of fields.
            (Type::Structural(lfields), Type::Structural(rfields))  => todo!(),
            (Type::Nil, other) => Err(TypeError::new_types_not_equal(&Type::Nil, &other)),
            (Type::Variable(_), other) => Err(TypeError::new_with_custom(format!("Cannot assign {other} to unknown type!").as_ref())),
            (lhs @ Type::Structural(_), rhs) => Err(TypeError::new_types_not_equal(&lhs, &rhs)),
            (lhs, Type::AnyValue) => Err(TypeError::new_with_custom(&format!("Unable to assign AnyValue to {lhs}"))),
        }
    }

    fn unify_structural_fields(&mut self, lfields: BTreeMap<String,Type>, rfields: BTreeMap<String, Type>) -> Result<Type, TypeError> {
        let mut result_fields = BTreeMap::new();
        for (rf, rt) in &rfields {
            if !&lfields.contains_key(rf) {
                result_fields.insert(rf.clone(), rt.clone());
            }
        }
        for (lf, lt) in lfields {
            match rfields.get(&lf) {
                Some(rt) => {
                    result_fields.insert(lf, self.unify(lt, rt.clone())?);
                },
                None => {
                    result_fields.insert(lf, lt);
                },
            }
        }
        Ok(Type::Structural(result_fields))
    }

    fn unify_concrete_to_struct_subtype(&mut self, concrete: Type, structure: &BTreeMap<String,Type>) -> Result<Type, TypeError> {
        // We recursively go through all of the fields in the structure and make
        // sure they exist in the concrete type.
        let structure_name = concrete.structure_name()?;
        for (field, field_type) in structure {
            let expected_type = self.scope.lookup_member(structure_name, field)?;
            self.unify_subtype(expected_type, field_type.clone())?;
        }
        Ok(concrete)
    }

    fn type_id(&mut self, name: String) -> Result<TypedExpr, TypeError> {
        let tpe = self.scope.lookup_id(&name)?;
        Ok(TypedExpr::Id(name, tpe))
    }

    fn type_binary_op(&mut self, lexpr: Box<Expr>, op: BinaryOperator, rexpr: Box<Expr>) -> Result<TypedExpr, TypeError> {
        match op {
            BinaryOperator::In => self.type_in_expr(lexpr, rexpr),
            BinaryOperator::Equals | BinaryOperator::NotEquals => self.type_boolean_expr(lexpr, op, rexpr),
            BinaryOperator::With => self.type_merge_expr(lexpr, rexpr),
            BinaryOperator::Divide | BinaryOperator::Minus | BinaryOperator::Plus | BinaryOperator::Times => self.type_numeric_expr(lexpr, op, rexpr),
        }
    }

    fn type_numeric_expr(&mut self, lexpr: Box<Expr>, op: BinaryOperator, rexpr: Box<Expr>) -> Result<TypedExpr, TypeError> {
        let ltype = self.type_expr(*lexpr)?;
        let rtype = self.type_expr(*rexpr)?;
        if !ltype.my_type().is_numeric() {
            Err(TypeError::new_with_custom(&format!("Expected numeric type for '{}', found: {}", op, ltype.my_type())))
        } else if !rtype.my_type().is_numeric() {
            Err(TypeError::new_with_custom(&format!("Expected numeric type for '{}', found: {}", op, rtype.my_type())))
        } else {
            // TODO - This may be too strict, may need to relax to allow coercions
            let result_type = self.unify(ltype.my_type(), rtype.my_type())?;
            Ok(TypedExpr::BinaryOp(Box::new(ltype), op, Box::new(rtype), result_type))
        }
    }

    fn type_boolean_expr(&mut self, lexpr: Box<Expr>, op: BinaryOperator, rexpr: Box<Expr>) -> Result<TypedExpr, TypeError> {
        let ltype = self.type_expr(*lexpr)?;
        let rtype = self.type_expr(*rexpr)?;
        // We ensure the comparison is of the same type, but ignore which type it is for now.
        self.unify(ltype.my_type(), rtype.my_type())?;
        Ok(TypedExpr::BinaryOp(Box::new(ltype), op, Box::new(rtype), Type::Boolean()))
    }

    fn type_in_expr(&mut self, value: Box<Expr>, list: Box<Expr>) -> Result<TypedExpr, TypeError> {
        let ltype = self.type_expr(*list)?;
        let etype = self.type_expr(*value)?;
        // Make sure the RHS can take a list of elements of the lhs.
        self.unify(ltype.my_type(), Type::List(etype.my_type()))?;
        Ok(TypedExpr::BinaryOp(Box::new(etype), BinaryOperator::In, Box::new(ltype), Type::Boolean()))
    }

    fn type_merge_expr(&mut self, lexpr: Box<Expr>, rexpr: Box<Expr>) -> Result<TypedExpr, TypeError> {
        let lhs = self.type_expr(*lexpr)?;
        let rhs = self.type_expr(*rexpr)?;
        // TODO - Force both LHS + RHS to be structural types of some fashion.

        // We only check that the RHS will "fit" in the LHS.
        let rtype = self.unify_subtype(lhs.my_type(), rhs.my_type())?;
        Ok(TypedExpr::BinaryOp(Box::new(lhs), BinaryOperator::With, Box::new(rhs), rtype))
    }

    fn type_list_expr(&mut self, values: Vec<Expr>) -> Result<TypedExpr, TypeError> {
        // Unify all the value types of the list.
        let vtypes = self.type_exprs(values)?;
        let mut etype = self.new_variable();
        for t in vtypes.iter() {
            etype = self.unify(etype, t.my_type())?;
        }
        let tpe = Type::List(etype);
        Ok(TypedExpr::List(vtypes, tpe))
    }

    fn type_accessor(&mut self, expr: Box<Expr>, member: String) -> Result<TypedExpr, TypeError> {
        let texpr = self.type_expr(*expr)?;
        match texpr.my_type() {
            Type::Constructor(name, _) => {
                let tpe = self.scope.lookup_member(&name, &member)?;
                Ok(TypedExpr::Accessor(Box::new(texpr), member, tpe))
            },
            // We don't know how to defer lookup on accessors, we MUST know this type ahead of time.
            _ => Err(TypeError::todo()),
        }
    }

    fn type_application(&mut self, func: Box<Expr>, args: Vec<Expr>) -> Result<TypedExpr, TypeError> {
        let atypes = self.type_exprs(args)?;
        let ftype = self.type_expr(*func)?;
        let rtype = self.new_variable();
        let rhs_ftpe = Type::Function(rtype, atypes.iter().map(|a| a.my_type()).collect());
        // Here, again, we can check assignability of rhs to lhs.
        let result = self.unify(ftype.my_type(), rhs_ftpe)?;
        Ok(TypedExpr::Application(Box::new(ftype), atypes, result.get_function_return_type()?))
    }

    fn type_structure_construction(&mut self, field_assignments: Vec<FieldAssignment>) -> Result<TypedExpr, TypeError> {
        let fields_or_err: Result<Vec<TypedFieldAssignment>, TypeError> = 
          field_assignments.into_iter()
          .map(|f| self.type_field_assignment(f))
          .collect();
        let fields = fields_or_err?;
        let field_types: BTreeMap<String, Type> =
          fields.iter()
          .map(|f| (f.field_name().into(), f.my_type()))
          .collect();
         Ok(TypedExpr::StructureConstruction(fields, Type::Structural(field_types)))
    }

    fn type_field_assignment(&mut self, field_assign: FieldAssignment) -> Result<TypedFieldAssignment, TypeError> {
        // TODO - consume FieldAssing isntead of cloning.
        let value_expr = self.type_expr(field_assign.expr().clone())?;
        Ok(TypedFieldAssignment(field_assign.name().into(), Box::new(value_expr)))
    }

    // Types a vector of expressions, returning the first error encountered, if any.
    fn type_exprs(&mut self, es: Vec<Expr>) -> Result<Vec<TypedExpr>, TypeError> {
        es.into_iter()
        .map(|e| self.type_expr(e))
        .collect()
    }

    // TODO - Result should probably be typed tree.
    pub fn type_expr(&mut self, e: Expr) -> Result<TypedExpr, TypeError> {
        match e {
            Expr::Id(n) => self.type_id(n),
            Expr::List(values) => self.type_list_expr(values),
            Expr::StructureConstruction(field_assigns) => self.type_structure_construction(field_assigns),
            Expr::BinaryOp(lexpr, op, rexpr) => self.type_binary_op(lexpr, op, rexpr),
            Expr::Accessor(expr, member) => self.type_accessor(expr, member),
            Expr::Application(func, args) => self.type_application(func, args),            
            Expr::Nil => Ok(TypedExpr::Nil),
            Expr::Bool(value) => Ok(TypedExpr::Bool(value)),
            Expr::String(value) => Ok(TypedExpr::String(value)),
            Expr::Int(value) => Ok(TypedExpr::Int(value)),
        }
    }

    // Constructs a new Typer that can type check expressions for a given stream type.
    fn new_typer_for(&self, id: StreamIdentifier) -> Typer {
        let mut scope: SymbolTable = self.scope.clone();
        match id {
            StreamIdentifier::Metric => scope.register_simple_term("metric", Type::Simple("Metric")),
            StreamIdentifier::Log => scope.register_simple_term("log", Type::Simple("Log")),
            StreamIdentifier::Span => scope.register_simple_term("span", Type::Simple("Span")),
            StreamIdentifier::SpanEvent => scope.register_simple_term("event", Type::Simple("SpanEvent")),
        }
        // TODO - inject symbols for the given stream type.
        Typer {
            // TODO - maybe current id needs to be safer?
            current_id: self.current_id,
            substitutions: self.substitutions.clone(),
            scope,
        }
    }

    // Figures out terms created by the extractor and gives them names and a type.
    fn type_extractor(&mut self, e: Extractor, t: Type) -> Result<(TypedExtractor, Vec<(String, Type)>), TypeError> {
        match e {
            Extractor::Term(id) => Ok((TypedExtractor::Term(id.clone(), t.clone()), vec!((id, t)))),
            Extractor::Pattern(name, args) => {
                // We expect patterns to be functions from t => Optional[args]
                let extractor_function_type = self.scope.lookup_id(&name)?;
                let unknown_result_type = self.new_variable();
                let extractor_function_type: Type = self.unify(extractor_function_type, Type::Function(unknown_result_type, vec!(t.clone())))?;
                match extractor_function_type.get_function_return_type()? {
                    Type::Constructor(name, targs) if name == "Option" && targs.len() == 1 => {
                        // Now we match on the argument value of Option to see if it's a tuple and extract
                        // more patterns.
                        match &targs[0] {
                            other if args.len() == 1 => {
                                let (targ, names)  = self.type_extractor(args[0].clone(), other.clone())?;
                                Ok((TypedExtractor::Pattern(name, vec!(targ), other.clone()), names))
                            },
                            // TODO - check for tuples of types and extract each one.
                            _ => todo!(),
                        }
                    },
                    other => Err(TypeError::new_with_custom(&format!("Pattern match expected extraction of {t} => Option[...], but found {other}"))),
                }
            },
        }
    }

    // Returns the typed pattern and a typer to use when typing underlying expressions.
    fn type_pattern(&mut self, pattern: PatternMatch) -> Result<(TypedPatternMatch, Typer), TypeError> {
        let texpr = self.type_expr(pattern.source().clone())?;
        // Figure out what terms are defined by the extraction and add them to the scope of the returned typer.
        let (tpattern, new_symbols) = self.type_extractor(pattern.pattern().clone(), texpr.my_type())?;
        let mut next_scope = self.scope.clone();
        for (name, tpe) in new_symbols {
            next_scope.terms.insert(name, tpe);
        }
        let next_typer = Typer {
            current_id: self.current_id,
            scope: next_scope,
            substitutions: self.substitutions.clone(),
        };
        Ok((TypedPatternMatch(Box::new(texpr), tpattern, None), next_typer))
    }

    pub fn type_statement(&mut self, e: Statement) -> Result<TypedStatement, TypeError> {
        match e {
            // Should we allow dropping everything?
            Statement(id, None, StatementType::Drop, None) => Ok(TypedStatement(id, None, TypedStatementType::Drop, None)),
            Statement(id, None, StatementType::Yield(expr), None) => {
                let mut next_typer = self.new_typer_for(id.clone());
                let texpr = next_typer.type_expr(*expr)?;
                // TODO - ensure texpr has no leaking type variables.
                Ok(TypedStatement(id, None, TypedStatementType::Yield(Box::new(texpr)), None))
            },
            Statement(id, Some(pattern), StatementType::Yield(expr), None) => {
                let mut next_typer = self.new_typer_for(id.clone());
                let (tpattern, mut expr_typer) = next_typer.type_pattern(pattern)?;
                let texpr = expr_typer.type_expr(*expr)?;
                // Make sure we don't have conflicting type variables going forward.
                self.current_id = std::cmp::max(self.current_id, expr_typer.current_id);
                // TODO - make sure we have no leaking type variables here.
                Ok(TypedStatement(id, Some(tpattern), TypedStatementType::Yield(Box::new(texpr)), None))
            },
            _ => todo!(),
        }
    }
}


// Helper method to check for recursive types, which are not allowed in our type system.
fn occurs_in(id: u64, tpe: &Type) -> bool {
    match tpe {
        Type::Variable(oid) => id == *oid,
        Type::Constructor(_, args) => args.iter().any(|t| occurs_in(id, t)),
        Type::Structural(fields) => fields.iter().any(|(_,t)| occurs_in(id, t)),
        Type::AnyValue => false,
        Type::Nil => false,
    }
}

// We always register standard types for OTTL.
fn register_standard_types(typer: &mut Typer) {
    let time_type = Type::Simple("Time");
    let span_id_type = Type::Simple("SpanID");
    let trace_id_type = Type::Simple("TraceID");

    // TODO - we need to register AnyValue as a union type but
    // it is self-recursive with itself, thanks to List[AnyValue]....
    // OR we can directly lift KvList/ArrayList into typesystem...

    // Span Status Type
    let mut span_status_fields = BTreeMap::new();
    span_status_fields.insert("code".into(), Type::Int());
    span_status_fields.insert("message".into(), Type::String());
    let span_status = StructSymbol {
        name: "SpanStatus".into(),
        fields: span_status_fields,
    };
    typer.scope_mut().register_structure(span_status);
    // Span Type
    let mut span_fields = BTreeMap::new();
    span_fields.insert("name".into(), Type::String());
    span_fields.insert("kind".into(), Type::Int());
    span_fields.insert("status".into(), Type::Simple("SpanStatus"));
    span_fields.insert("startTime".into(), time_type.clone());
    span_fields.insert("endTime".into(), time_type.clone());
    span_fields.insert("spanID".into(), span_id_type.clone());
    span_fields.insert("traceID".into(), trace_id_type.clone());
    // TODO - attributes
    // TODO - events
    // TODO - dropped_*
    let span: StructSymbol = StructSymbol {
        name: "Span".into(),
        fields: span_fields,
    };
    typer.scope_mut().register_structure(span);
    typer.scope_mut().register_structure(StructSymbol {
        name: "Metric".into(),
        fields: [
            ("name".into(), Type::String()),
            ("description".into(), Type::String()),
            ("unit".into(), Type::String()),
            ("sum".into(), Type::Simple("Sum")),
        ].into(),
    });

    // Here we register standard extractors.
    typer.scope_mut().register_simple_term(
        "aSum", 
        Type::Function(Type::Constructor("Option".into(), vec!(Type::Simple("Sum"))), vec!(Type::Simple("Metric")))
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{mk_parser_input, parse_expr, parse_statement};


    #[test]
    fn test_display_of_types() {
        // TODO - We may want to just always print variables as <unknown-type {id}> or some such for better errors.
        assert_eq!(format!("{}", Type::Variable(24)), "?24");
        assert_eq!(format!("{}", Type::Simple("Int")), "Int");
        assert_eq!(format!("{}", Type::List(Type::Simple("Int"))), "List[Int]");
        assert_eq!(format!("{}", Type::ArrayValue()), "ArrayValue"); // Not List[AnyValue]
        assert_eq!(format!("{}", Type::Nil), "Nil");
        assert_eq!(format!("{}", Type::AnyValue), "AnyValue");
        // TODO - We want this to be "(ArgOne, ArgTwo) Return"
        assert_eq!(format!("{}", Type::Function(Type::Simple("Return"), vec!(Type::Simple("ArgOne"), Type::Simple("ArgTwo")))), "(ArgOne, ArgTwo) Return");
        assert_eq!(format!("{}", Type::Structural(BTreeMap::from([("a".into(), Type::Int()), ("b".into(), Type::AnyValue)]))), "{a:Int, b:AnyValue}");
    }

    fn parse_statement_and_type(typer: &mut Typer, input: &str) -> Result<TypedStatement, TypeError> {
        let input = mk_parser_input(input);
        let parse_result = parse_statement(input);
        match parse_result {
            Ok((rest, ast)) => {
                assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
                Ok(typer.type_statement(ast)?)
            },
            Err(err) => Err(TypeError::new_with_custom(&format!("{:?}", err))),
        }
    }


    fn parse_and_type(typer: &mut Typer, input: &str) -> Result<Type, TypeError> {
        let input = mk_parser_input(input);
        let parse_result = parse_expr(input);
        match parse_result {
            Ok((rest, ast)) => {
                assert_eq!(AsRef::<str>::as_ref(&rest), "", "Did not fully parse input!");
                Ok(typer.type_expr(ast)?.my_type())
            },
            Err(err) => Err(TypeError::new_with_custom(&format!("{:?}", err))),
        }
    }

    #[test]
    fn test_pattern_match() {
        let mut typer = Typer::new(SymbolTable::new());
        let result: TypedStatement = parse_statement_and_type(&mut typer, "on metric when metric is aSum(sum) yield metric with { sum: sum }").unwrap();
        // TODO - write a real test here.
        assert_eq!(result.stream_type(), Type::Simple("Metric"));
        panic!("{result:?}")
    }

    #[test]
    fn test_in_expr_types() {
        // We should succesfully parse and type an expression when looking for an element in a list.
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Simple("Int"));
        symbols.register_simple_term("y", Type::List(Type::Simple("Int")));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "x in y").unwrap();
        assert_eq!(result, Type::Boolean());
    }
    #[test]
    fn test_in_expr_fails_not_list() {
        // We should succesfully parse and type an expression when looking for an element in a list.
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Simple("Int"));
        symbols.register_simple_term("y", Type::Simple("String"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "x in y");
        assert!(result.is_err());
    }

    #[test]
    fn test_equals_expr_fails_on_different_types() {
        // We should succesfully parse and type an expression when looking for an element in a list.
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Simple("Int"));
        symbols.register_simple_term("y", Type::Simple("String"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "x == y");
        assert!(result.is_err());
    }
    #[test]
    fn test_equals_expr_types() {
        // We should succesfully parse and type an expression when looking for an element in a list.
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Simple("Int"));
        symbols.register_simple_term("y", Type::Simple("Int"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "x == y").unwrap();
        assert_eq!(result, Type::Boolean());
    }

    #[test]
    fn test_accessor_types() {
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("x", Type::Simple("SpanStatus"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "x.code").unwrap();
        assert_eq!(result, Type::Simple("Int"));
    }

    #[test]
    fn test_nil() {
        let symbols = SymbolTable::new();
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "nil").unwrap();
        assert_eq!(result, Type::Nil)
    }

    #[test]
    fn test_structure_construction() {
        let symbols = SymbolTable::new();
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "{ status: { code: 1 } }").unwrap();
        assert_eq!(result, Type::Structural(
            BTreeMap::from([("status".into(), Type::Structural(
                BTreeMap::from([("code".into(), Type::Int())])
            ))])
        ))
    }

    #[test]
    fn test_structure_construction_and_merge() {
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("self", Type::Simple("Span"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "self with { status: { code: nil } }").unwrap();
        assert_eq!(result, Type::Simple("Span"))
    }

    #[test]
    fn test_structure_construction_and_merge_fails() {
        let mut symbols = SymbolTable::new();
        symbols.register_simple_term("self", Type::Simple("Span"));
        let mut typer = Typer::new(symbols);
        let result = parse_and_type(&mut typer, "self with { status: { coder: nil } }");
        assert!(result.is_err())
    }
}