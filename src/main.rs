use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{cut, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, parser_ext::ParserExt, tag::complete::tag};

type ParseError<I> = ErrorTree<I>;
// Note that IResult<I, O> = Result<(I, O), WrappedParseError<I>>
type WrappedParseError<I> = nom::Err<ParseError<I>>;
type ParseResult<I, O> = IResult<I, O, ParseError<I>>;
type TryParseResult<I, O> = Result<Result<(I, O), O>, WrappedParseError<I>>;
trait Parse<I>
where
    Self: Sized,
{
    fn parse(i: I) -> ParseResult<I, Self>;
}
trait ParseAs
where
    Self: Sized,
{
    fn parse_as<O: Parse<Self>>(&self) -> Result<O, WrappedParseError<Self>>;
}
impl<'a> ParseAs for &'a str {
    fn parse_as<O: Parse<Self>>(&self) -> Result<O, WrappedParseError<Self>> {
        Ok(O::parse(self)?.1)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum UnOp {
    Neg,
    Not,
}
impl Parse<&str> for UnOp {
    fn parse(i: &str) -> ParseResult<&str, Self> {
        use UnOp::*;
        alt((value(Neg, char('-')), value(Not, char('!'))))(i)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
enum Op {
    Plus,
    Minus,
    Times,
    Divide,
}
impl Parse<&str> for Op {
    fn parse(i: &str) -> ParseResult<&str, Self> {
        use Op::*;
        alt((
            value(Plus, char('+')),
            value(Minus, char('-')),
            value(Times, char('*')),
            value(Divide, char('/')),
        ))(i)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Literal {
    Int(i32),
    Bool(bool),
    Str(String),
}
impl Literal {
    fn parse_int(i: &str) -> ParseResult<&str, Literal> {
        alt((digit1, tag("-").and(digit1).recognize()))
            .map_res(|s| i32::from_str_radix(s, 10).map(Literal::Int))
            .parse(i)
    }
    fn parse_bool(i: &str) -> ParseResult<&str, Literal> {
        alt((
            value(Literal::Bool(true), tag("true")),
            value(Literal::Bool(false), tag("false")),
        ))(i)
    }
    fn parse_str(i: &str) -> ParseResult<&str, Literal> {
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
            cut(tag("\"")),
        )
        .map(Literal::Str)
        .parse(i)
    }
}
impl Parse<&str> for Literal {
    fn parse(i: &str) -> ParseResult<&str, Self> {
        alt((Literal::parse_bool, Literal::parse_int, Literal::parse_str))(i)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Expr {
    UnOp(UnOp, Box<Expr>),
    Literal(Literal),
    Id(String),
    Op(Box<Expr>, Op, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}
impl Expr {
    fn parse_unop(i: &str) -> ParseResult<&str, Self> {
        pair(terminated(UnOp::parse, multispace0), Expr::parse_atomic)
            .map(|(u, x)| Expr::UnOp(u, Box::new(x)))
            .parse(i)
    }
    fn parse_id(i: &str) -> ParseResult<&str, Expr> {
        pair(
            alt((alpha1::<&str, _>, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )
        .recognize()
        .map(|s| Expr::Id(s.to_string()))
        .parse(i)
    }
    fn parse_literal(i: &str) -> ParseResult<&str, Self> {
        Literal::parse.map(Expr::Literal).parse(i)
    }
    fn parse_parens(i: &str) -> ParseResult<&str, Expr> {
        delimited(char('('), Expr::parse, char(')'))(i)
    }
    fn parse_atomic(i: &str) -> ParseResult<&str, Self> {
        alt((
            Expr::parse_parens,
            Expr::parse_unop,
            Expr::parse_literal,
            Expr::parse_id,
        ))(i)
    }
    fn parse_tuple(i: &str) -> ParseResult<&str, Vec<Expr>> {
        delimited(
            char('('),
            separated_list0(char(','), Expr::parse.delimited_by(multispace0)),
            char(')'),
        )(i)
    }
    fn try_parse_op_seq(i: &str, first: Expr) -> TryParseResult<&str, Expr> {
        // Parse a sequence of operator-separated atomic expressions
        // If parse fails, return `first` and a ParseError
        enum Unit {
            Atomic(Expr),
            BinOp(Box<Unit>, Op, Box<Unit>),
        }
        enum State {
            Complete(Unit),       // A valid expression, can be followed by an Op
            Incomplete(Unit, Op), // A fragment ending in an Op
        }
        use State::*;
        use Unit::*;
        fn unwind_first(
            err: WrappedParseError<&str>,
            unit: Unit,
        ) -> Result<(&str, State), (Expr, WrappedParseError<&str>)> {
            match unit {
                Atomic(x) => Err((x, err)),
                BinOp(b1, _op, _b2) => unwind_first(err, *b1),
            }
        }
        fn get_expr(unit: Unit) -> Expr {
            match unit {
                Atomic(x) => x,
                BinOp(b1, op, b2) => Expr::Op(Box::new(get_expr(*b1)), op, Box::new(get_expr(*b2))),
            }
        }
        fn parse_op_internal<'a>(
            mut rest: &'a str,
            mut state: State,
        ) -> Result<(&'a str, Expr), (Expr, WrappedParseError<&'a str>)> {
            loop {
                (rest, state) = match state {
                    Complete(unit @ Atomic(..)) => {
                        // Attempt to get an Op
                        match Op::parse.preceded_by(multispace0).parse(rest) {
                            Ok((new_rest, op)) => Ok((new_rest, Incomplete(unit, op))),
                            // If there's an error, we just return the atomic expression
                            Err(err) => unwind_first(err, unit),
                        }
                    }
                    Incomplete(b, op) => {
                        // Attempt to get an atomic
                        match cut(Expr::parse_atomic.preceded_by(multispace0))(rest) {
                            Ok((new_rest, atomic)) => Ok((
                                new_rest,
                                Complete(BinOp(Box::new(b), op, Box::new(Atomic(atomic)))),
                            )),
                            Err(err) => unwind_first(err, b),
                        }
                    }
                    Complete(BinOp(b1, op, b2)) => match Op::parse
                        .preceded_by(multispace0)
                        .opt()
                        .parse(rest)
                    {
                        Ok((new_rest, maybe_op)) => match maybe_op {
                            // No more to parse => convert unit to an Expr and exit
                            None => return Ok((new_rest, get_expr(BinOp(b1, op, b2)))),
                            Some(new_op) => {
                                if new_op <= op {
                                    // Lower priority op means we can regard our work so far as encapsulated
                                    Ok((new_rest, Incomplete(BinOp(b1, op, b2), new_op)))
                                } else {
                                    // Higher priority op means we must recurse
                                    match parse_op_internal(new_rest, Incomplete(*b2, new_op)) {
                                        Ok((new_rest, x)) => {
                                            return Ok((
                                                new_rest,
                                                Expr::Op(Box::new(get_expr(*b1)), op, Box::new(x)),
                                            ))
                                        }
                                        Err((_, err)) => unwind_first(err, *b1),
                                    }
                                }
                            }
                        },
                        Err(err) => unwind_first(err, *b1),
                    },
                }?;
            }
        }
        match parse_op_internal(i, Complete(Atomic(first))) {
            // Parser succeeded
            Ok(res) => Ok(Ok(res)),
            Err((x, err)) => match err {
                // Parser failed unrecoverably: pass the Failure error along
                nom::Err::Failure(..) => Err(err),
                // Parser failed recoverably: give back the first atomic to the general Expr parser
                _ => Ok(Err(x)),
            },
        }
    }
    fn try_parse_call(rest: &str, first: Expr) -> TryParseResult<&str, Expr> {
        match Expr::parse_tuple.preceded_by(multispace0).parse(rest) {
            Ok((rest, args)) => Ok(Ok((rest, Expr::Call(Box::new(first), args)))),
            Err(err) => match err {
                nom::Err::Failure(..) => Err(err),
                _ => Ok(Err(first)),
            },
        }
    }
    fn try_parse_if(rest: &str, first: Expr) -> TryParseResult<&str, Expr> {
        match preceded(
            multispace0.and(char('?')),
            cut(Expr::parse_atomic.delimited_by(multispace0)),
        )
        .and(cut(preceded(
            char(':'),
            Expr::parse_atomic.delimited_by(multispace0),
        )))
        .parse(rest)
        {
            Ok((rest, (yes, no))) => Ok(Ok((
                rest,
                Expr::If(Box::new(first), Box::new(yes), Box::new(no)),
            ))),
            Err(err) => match err {
                nom::Err::Failure(..) => Err(err),
                _ => Ok(Err(first)),
            },
        }
    }
}
impl Parse<&str> for Expr {
    fn parse(i: &str) -> ParseResult<&str, Self> {
        let (rest, expr) = Expr::parse_atomic(i)?;
        let expr = match Expr::try_parse_op_seq(rest, expr)? {
            // Parsed an operator sequence => terminate and return
            Ok(res) => return Ok(res),
            // Recoverably failed => continue trying non-atomic patterns
            Err(expr) => expr,
        };
        let expr = match Expr::try_parse_call(rest, expr)? {
            Ok(res) => return Ok(res),
            Err(expr) => expr,
        };
        let expr = match Expr::try_parse_if(rest, expr)? {
            Ok(res) => return Ok(res),
            Err(expr) => expr,
        };
        Ok((rest, expr))
    }
}

fn main() {
    match "troo(2 * six - 1, true ? 3 : (2 - 3))".parse_as::<Expr>() {
        Ok(expr) => println!("{:?}", expr),
        Err(err) => println!("failed to parse: {:?}", err),
    }
}
