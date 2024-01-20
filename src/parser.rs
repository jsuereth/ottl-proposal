mod string;

use crate::ast::*;
use nom::{
    IResult,
    bytes::complete::tag,
    branch::alt,
    multi::{many0_count, separated_list0},
    combinator::{recognize, value},
    sequence::pair,
    character::complete::{alpha1, alphanumeric1, multispace0, i64},
};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser,RecursiveInfo};

// Our new input type.
type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

// Copied from nom patterns
fn parse_identifier_raw(input: Span) -> IResult<Span, String> {
    // TODO - prevent protected keywords:
    // in, where, from
    let (input, result) = recognize(
      pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_"))))
      )
    )(input)?;
    let r: &str = result.as_ref();
    Ok((input, r.to_string()))
}

// TODO - we use left-recursive macro that's a bit suspect and requires var-names to be `s`
// We should look at refactoring this grammar to not be left recursive or write our own parser.

// TODO - less hacky parser, e.g. handling \r\n consistently

fn parse_identifer(input: Span) -> IResult<Span, Expr> {
    let (input, name) = parse_identifier_raw(input)?;
    Ok((input, Expr::Id(name.to_string())))
}

fn parse_list_separator(input: Span) -> IResult<Span, ()> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, ()))
}

fn parse_list(input: Span) -> IResult<Span, Expr> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, result) = separated_list0(parse_list_separator, parse_expr)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("]")(input)?;
    Ok((input, Expr::List(result)))
}

#[recursive_parser]
fn parse_accessor(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = parse_expr(s)?;
    let (s, _) = tag(".")(s)?;
    let (s, member) = parse_identifier_raw(s)?;
    Ok((s, Expr::Accessor(Box::new(expr), member.to_string())))
}

#[recursive_parser]
fn parse_application(s: Span) -> IResult<Span, Expr> {
    let (s, expr) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("(")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, args) = separated_list0(parse_list_separator, parse_expr)(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag(")")(s)?;
    Ok((s, Expr::Application(Box::new(expr), args)))
}

fn parse_field_assignment(s: Span) -> IResult<Span, FieldAssignment> {
    let (s, name) = parse_identifier_raw(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag(":")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, value) = parse_expr(s)?;
    Ok((s, FieldAssignment::new(name, value)))
}

fn parse_structure_construction(s: Span) -> IResult<Span, Expr> {
    let (s, _) = tag("{")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, fields) = separated_list0(parse_list_separator, parse_field_assignment)(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("}")(s)?;
    Ok((s, Expr::StructureConstruction(fields)))
}



fn parse_binary_operator(s: Span) -> IResult<Span, BinaryOperator> {
    alt((
        value(BinaryOperator::Equals, tag("==")),
        value(BinaryOperator::NotEquals, tag("!=")),
        value(BinaryOperator::In, tag("in")),
        value(BinaryOperator::Plus, tag("+")),
        value(BinaryOperator::Minus, tag("-")),
        value(BinaryOperator::Times, tag("*")),
        value(BinaryOperator::Divide, tag("/")),
        value(BinaryOperator::With, tag("with")),
    ))(s)
}

#[recursive_parser]
fn parse_binary_expression(s: Span) -> IResult<Span, Expr> {
    let (s, lexpr) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, op) = parse_binary_operator(s)?;
    let (s, _) = multispace0(s)?;
    let (s, rexpr) = parse_expr(s)?;
    Ok((s, Expr::BinaryOp(Box::new(lexpr), op, Box::new(rexpr))))
}

fn parse_nil(s: Span) -> IResult<Span, Expr> {
    value(Expr::Nil, tag("nil"))(s)
}

fn parse_int(s: Span) -> IResult<Span, Expr> {
    let (s, value) = i64(s)?;
    Ok((s, Expr::Int(value)))
}

fn parse_bool(s: Span) -> IResult<Span, Expr> {
    alt((
        value(Expr::Bool(true), tag("true")),
        value(Expr::Bool(false), tag("false")),
    ))(s)
}

// TODO - Figure out.
// fn parse_string(s: Span) -> IResult<Span, Expr> {
//     let (s, value) = string::parse_string(&s)?;
//     Ok((s, Expr::String(value)))
// }

pub fn parse_expr(input: Span) -> IResult<Span, Expr> {
    // Priority of these matter! we need to greedily have longest first.
    alt((
        parse_application,
        parse_binary_expression,
        parse_accessor,
        parse_structure_construction,
        parse_list,
        parse_nil,
        parse_int,
        parse_bool,
        parse_identifer)
    )(input)
}

fn parse_stream_identifier(input: Span) -> IResult<Span, StreamIdentifier> {
    alt((
        value(StreamIdentifier::Metric, tag("metric")),
        value(StreamIdentifier::Log, tag("log")),
        value(StreamIdentifier::Span, tag("span")),
        value(StreamIdentifier::SpanEvent, tag("spanevent")),
    ))(input)
}

pub fn parse_stream(input: Span) -> IResult<Span, Stream> {
    let (input, _) = tag("from")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, id) = parse_stream_identifier(input)?;
    let (input, _) = multispace0(input)?;
    // TODO - optional where expression
    let (input, _) = tag("yield")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = parse_expr(input)?;
    Ok((input, Stream::new(id, expr, None)))
}

pub fn mk_parser_input(input: &str) -> Span {
    LocatedSpan::new_extra(input, RecursiveInfo::new())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;


    fn fully_parse<A, F>(parser: F, input: &str) -> A 
      where F: Fn(Span) -> IResult<Span, A> {
        let real_input = mk_parser_input(input);
        let (unparsed, result) = parser(real_input).unwrap();
        assert_eq!(AsRef::<str>::as_ref(&unparsed), "", "Did not fully parse input!");
        result
    }

    #[test]
    fn test_parse_name() {
        let (unparsed, result) = parse_identifer(mk_parser_input("foo bar")).unwrap();
        assert_eq!(AsRef::<str>::as_ref(&unparsed), " bar");
        assert_eq!(result, Expr::Id("foo".to_string()));
    }

    #[test]
    fn test_parse_list() {
        let result = fully_parse(parse_expr, "[foo,bar]");
        assert_eq!(result, Expr::List(vec!(Expr::Id("foo".to_string()), Expr::Id("bar".to_string()))));

        let result = fully_parse(parse_expr, "[ foo , bar ]");
        assert_eq!(result, Expr::List(vec!(Expr::Id("foo".to_string()), Expr::Id("bar".to_string()))));
    }

    #[test]
    fn test_parse_accessor() {
        let result = fully_parse(parse_expr, "foo.bar");
        assert_eq!(result, Expr::Accessor(Box::new(Expr::Id("foo".to_string())), "bar".to_string()));
    }

    #[test]
    fn test_parse_application() {
        let result = fully_parse(parse_expr, "bar(baz, biz)");
        assert_eq!(result, 
            Expr::Application(
              Box::new(Expr::Id("bar".to_string())),
              vec!(
                Expr::Id("baz".to_string()),
                Expr::Id("biz".to_string()),
              )
            ));

        let result = fully_parse(parse_expr, "foo.bar(baz, biz)");
        assert_eq!(result, 
            Expr::Application(
              Box::new(Expr::Accessor(Box::new(Expr::Id("foo".to_string())), "bar".to_string())),
              vec!(
                Expr::Id("baz".to_string()),
                Expr::Id("biz".to_string()),
              )
            ));
    }

    #[test]
    fn test_parse_structure() {
        let result = fully_parse(parse_expr, "{ foo: bar, biz: baz }");
        assert_eq!(result, 
            Expr::StructureConstruction(
                vec!(
                    FieldAssignment::new("foo".to_string(), Expr::Id("bar".to_string())),
                    FieldAssignment::new("biz".to_string(), Expr::Id("baz".to_string())),
                )
            ));
    }


    #[test]
    fn test_parse_nested_structure() {
        let result = fully_parse(parse_expr, "{ foo: { biz: baz } }");
        assert_eq!(result, 
            Expr::StructureConstruction(
                vec!(
                    FieldAssignment::new("foo".to_string(),
                        Expr::StructureConstruction(
                            vec!(
                                FieldAssignment::new("biz".to_string(), Expr::Id("baz".to_string())),
                            )
                        )
                    )
                ),
            ));
    }

    #[test]
    fn test_parse_boolean_expression() {
        let result = fully_parse(parse_expr, "foo == bar");
        assert_eq!(result, Expr::BinaryOp(
            Box::new(Expr::Id("foo".to_string())),
            BinaryOperator::Equals,
            Box::new(Expr::Id("bar".to_string())),
        ));
        let result = fully_parse(parse_expr, "foo in bar");
        assert_eq!(result, Expr::BinaryOp(
            Box::new(Expr::Id("foo".to_string())),
            BinaryOperator::In,
            Box::new(Expr::Id("bar".to_string())),
        ));
        let result = fully_parse(parse_expr, "foo != bar");
        assert_eq!(result, Expr::BinaryOp(
            Box::new(Expr::Id("foo".to_string())),
            BinaryOperator::NotEquals,
            Box::new(Expr::Id("bar".to_string())),
        ));
        let result = fully_parse(parse_expr, "foo with bar");
        assert_eq!(result, Expr::BinaryOp(
            Box::new(Expr::Id("foo".to_string())),
            BinaryOperator::With,
            Box::new(Expr::Id("bar".to_string())),
        ));
    }
}