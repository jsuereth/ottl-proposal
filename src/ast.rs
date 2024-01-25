
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
    StructureConstruction(Vec<FieldAssignment>),            // '{' {fields} '}'
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
pub enum StatementType {
    Drop,
    Yield(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Extractor {
    // A term to use to identify the matched content.
    Term(String),
    // An extraction function and arguments/terms to use from its extraction.
    Pattern(String, Vec<Extractor>),
}

// On {Box<Expr>} As {Pattern} (if <Expr>)?
#[derive(Clone, Debug, PartialEq)]
pub struct PatternMatch(Box<Expr>, Extractor, Option<Expr>);

impl PatternMatch {
    pub fn new(expr: Expr, extractor: Extractor, condition: Option<Expr>) -> PatternMatch {
        PatternMatch(Box::new(expr), extractor, condition)
    }
    pub fn source(&self) -> &Expr {
        &self.0
    }
    pub fn pattern(&self) -> &Extractor {
        &self.1
    }
    pub fn condition(&self) -> Option<&Expr> {
        self.2.as_ref()
    }
}

// Statement({context}, {pattern}?, {eval}, {where-expr})
#[derive(Clone, Debug, PartialEq)]
pub struct Statement(pub StreamIdentifier, pub Option<PatternMatch>, pub StatementType, pub Option<Expr>);

impl Statement {
    pub fn new(id: StreamIdentifier, patern: Option<PatternMatch>, expr: StatementType, filter: Option<Expr>) -> Statement {
        Statement(id, patern, expr, filter)
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

impl std::fmt::Display for StreamIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StreamIdentifier::Log => write!(f, "log"),
            StreamIdentifier::Metric => write!(f, "metric"),
            StreamIdentifier::Span => write!(f, "span"),
            StreamIdentifier::SpanEvent => write!(f, "spanevent"),
        }
    }
}