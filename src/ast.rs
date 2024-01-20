
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOperator {
    Equals,     // =
    NotEquals,  // !=
    In,         // in
    Plus,       // +
    Minus,      // -
    Times,      // *
    Divide,     // /
    With,       // with
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Id(String),                                             // {identifier}
    List(Vec<Expr>),                                        // '[' {expr}* ']'
    StructureConstruction(Vec<FieldAssignment>), // '{' {fields} '}'
    Nil,                                                    // 'nil'
    Bool(bool),                                             // ('true' | 'false')
    String(String),                                         // {string}
    Int(i64),                                               // {integer}
    // Ambiguous & left-recursive asts.
    Accessor(Box<Expr>, String),                            // {expr} '.' {identifier}
    BinaryOp(Box<Expr>, BinaryOperator, Box<Expr>),         // {expr} op {expr}
    Application(Box<Expr>, Vec<Expr>),                      // {expr} '(' {arguments} ')'
}

#[derive(Clone, Debug, PartialEq)]
pub struct FieldAssignment(String, Box<Expr>);

impl FieldAssignment {
    pub fn new(name: String, expr: Expr) -> FieldAssignment {
        FieldAssignment(name, Box::new(expr))
    }

    pub fn name(&self) -> &str {
        &self.0
    }
    pub fn expr(&self) -> &Expr {
        &self.1
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stream(StreamIdentifier, Box<Expr>, Option<Expr>);

impl Stream {
    pub fn new(id: StreamIdentifier, expr: Expr, filter: Option<Expr>) -> Stream {
        Stream(id, Box::new(expr), filter)
    }
}

// Denotes one of the OTTL stream types.
#[derive(Clone, Debug, PartialEq)]
pub enum StreamIdentifier {
    Log,
    Metric,
    Span,
    SpanEvent,
}


// Implementations
impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Equals => write!(f, "=="),
            BinaryOperator::NotEquals => write!(f, "!="),
            BinaryOperator::In => write!(f, "in"),
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Times => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::With => write!(f, "with"),
        }
    }
}