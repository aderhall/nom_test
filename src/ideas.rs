use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, map_res, recognize, value},
    error::VerboseError,
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

/*
# Statements
- let identifier = expr;
- expr;
- if (expr) {code}
- if (expr) {expr} else {code}
- while (expr) {code}
- for (let_statement|expr; expr; expr) {code}
# Expressions
- expr(expr)
- (expr)
- {code}
- alt_seq[expr, binop]
- literal (num, bool, string)
*/

trait Parse: Sized {
    fn parse(i: &str) -> IResult<&str, Self, VerboseError<&str>>;
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Literal {
    Int(i32),
    Bool(bool),
    Str(String),
}
impl Literal {
    fn parse_int(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
        map_res(alt((digit1, recognize(tag("-").and_then(digit1)))), |s| {
            i32::from_str_radix(s, 10).map(|x| Literal::Int(x))
        })(i)
    }

    fn parse_bool(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
        use Literal::Bool;
        alt((
            value(Bool(true), tag("true")),
            value(Bool(false), tag("false")),
        ))(i)
    }

    fn parse_str(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
        map(
            delimited(
                tag("\""),
                escaped_transform(
                    is_not("\\\""),
                    '\\',
                    alt((
                        value("\\", tag("\\")),
                        value("\"", tag("\"")),
                        value("\n", tag("n")),
                        value("\t", tag("t")),
                        value("\r", tag("r")),
                    )),
                ),
                tag("\""),
            ),
            Literal::Str,
        )(i)
    }
}
impl Parse for Literal {
    fn parse(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
        alt((Literal::parse_int, Literal::parse_bool, Literal::parse_str))(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum BinOp {
    Plus,
    Minus,
    Times,
    Divide,
    DEquals,
}
impl Parse for BinOp {
    fn parse(i: &str) -> IResult<&str, Self, VerboseError<&str>> {
        use BinOp::*;
        alt((
            value(Plus, tag("+")),
            value(Minus, tag("-")),
            value(Times, tag("*")),
            value(Divide, tag("/")),
            value(DEquals, tag("==")),
        ))(i)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum UnOp {
    Not,
    Neg,
}
impl Parse for UnOp {
    fn parse(i: &str) -> IResult<&str, Self, VerboseError<&str>> {
        use UnOp::*;
        alt((value(Not, tag("!")), value(Neg, tag("-"))))(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expr {
    Literal(Literal),
    Id(String),
    BinOp(BinOp),
    UnOp(UnOp, Box<Expr>),
    OpSeq(Vec<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}
impl Expr {
    fn parse_id(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s| Expr::Id(String::from(s)),
        )(i)
    }
    fn parse_parens(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
        delimited(
            terminated(tag("("), multispace0),
            Expr::parse,
            preceded(multispace0, tag(")")),
        )(i)
    }
    fn parse_atomic(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
        alt((
            map(Literal::parse, Expr::Literal),
            Expr::parse_parens,
            Expr::parse_id,
        ))(i)
    }
    fn parse_tuple(i: &str) -> IResult<&str, Vec<Expr>, VerboseError<&str>> {
        delimited(
            terminated(tag("("), multispace0),
            separated_list0(
                terminated(tag(","), multispace0),
                terminated(Expr::parse, multispace0),
            ),
            tag(")"),
        )(i)
    }
    fn parse_call(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
        map(
            pair(Expr::parse_atomic, Expr::parse_tuple),
            |(func, args)| Expr::Call(Box::new(func), args),
        )(i)
    }
    fn parse_unop(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
        map(pair(UnOp::parse, Expr::parse_atomic), |(unop, expr)| {
            Expr::UnOp(unop, Box::new(expr))
        })(i)
    }
    fn parse_seq(i: &str) -> IResult<&str, Vec<Expr>, VerboseError<&str>> {
        separated_list1(preceded(multispace0, BinOp::parse), Expr::parse)(i)
    }
}
impl Parse for Expr {
    // TODO: first parse to a hierarchy of atomic expressions and sequences of those, then parse them to the larger structures like calls and operation sequences
    fn parse(i: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(BinOp::parse, Expr::BinOp),
            Expr::parse_call,
            Expr::parse_unop,
            Expr::parse_atomic,
        ))(i)
    }
}

fn unop_test(i: &str) -> IResult<&str, &str> {
    recognize(pair(tag("-"), alpha1))(i)
}
fn seq_test(i: &str) -> IResult<&str, Vec<&str>> {
    separated_list0(one_of("+-"), alt((alpha1, unop_test)))(i)
}

fn main() {
    let test_str = "(((abc + def) - 26) + 2)(hello, goodbye)";
    match seq_test(test_str) {
        Ok((rest, m)) => println!("{:?} ({:?} remaining)", m, rest),
        Err(e) => println!("{:?}", e),
    }
    //let test_str = "(22)(55, 44, b)";
    //match Expr::parse(test_str) {
    //    Ok((_, m)) => println!("{:?}", m),
    //    Err(e) => println!("{:?}", e),
    //}
}
