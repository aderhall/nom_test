use std::ops::RangeBounds;

use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{map, map_res, recognize, value, verify},
    error::{FromExternalError, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    AsBytes, AsChar, Compare, ExtendInto, FindSubstring, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Offset, ParseTo, Parser, Slice,
};

trait Parse<I, E>: Sized {
    fn parse(i: I) -> IResult<I, Self, E>;
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
impl<'a> Parse<&'a str, VerboseError<&'a str>> for Literal {
    fn parse(i: &str) -> IResult<&str, Literal, VerboseError<&str>> {
        alt((Literal::parse_int, Literal::parse_bool, Literal::parse_str))(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Ord)]
enum Op {
    Plus,
    Minus,
    Times,
    Divide,
    DEquals,
    Comma,
    Not,
}
impl Op {
    fn rank(&self) -> usize {
        use Op::*;
        match self {
            DEquals => 0,
            Comma => 1,
            Plus | Minus => 2,
            Times | Divide => 3,
            Not => 4,
        }
    }
}
impl<'a> Parse<&'a str, VerboseError<&'a str>> for Op {
    fn parse(i: &str) -> IResult<&str, Self, VerboseError<&str>> {
        use Op::*;
        alt((
            value(Plus, tag("+")),
            value(Minus, tag("-")),
            value(Times, tag("*")),
            value(Divide, tag("/")),
            value(DEquals, tag("==")),
            value(Comma, tag(",")),
            value(Not, tag("!")),
        ))(i)
    }
}
impl PartialOrd for Op {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.rank().cmp(&other.rank()))
    }
}

#[derive(Debug, Clone, Eq)]
struct Seq<'a> {
    els: &'a Vec<PreExpr>,
    range: std::ops::Range<usize>,
}
impl<'a> PartialEq for Seq<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            false
        } else {
            self.els
                .iter()
                .zip(other.els.iter())
                .all(|(x1, x2)| *x1 == *x2)
        }
    }
}
impl<'a> Seq<'a> {
    fn len(&self) -> usize {
        self.range.end - self.range.start
    }
    fn new(v: &'a Vec<PreExpr>) -> Self {
        Self {
            range: std::ops::Range {
                start: 0,
                end: v.len(),
            },
            els: v,
        }
    }
}
impl<'a> AsBytes for Seq<'a> {
    fn as_bytes(&self) -> &[u8] {
        Default::default()
    }
}
impl<'a, 'b> Compare<Seq<'b>> for Seq<'a> {
    fn compare(&self, t: Seq<'b>) -> nom::CompareResult {
        if *self == t {
            nom::CompareResult::Ok
        } else {
            nom::CompareResult::Error
        }
    }
    fn compare_no_case(&self, t: Seq<'b>) -> nom::CompareResult {
        self.compare(t)
    }
}
impl<'a> ExtendInto for Seq<'a> {
    type Item = PreExpr;
    type Extender = Vec<&'a PreExpr>;
    fn new_builder(&self) -> Self::Extender {
        self.els.iter().collect()
    }
    fn extend_into(&self, acc: &mut Self::Extender) {
        acc.extend(self.els.iter())
    }
}
impl<'a, 'b> FindSubstring<Seq<'b>> for Seq<'a> {
    fn find_substring(&self, substr: Seq<'b>) -> Option<usize> {
        self.els[self.range.clone()]
            .windows(substr.els.len())
            .position(|w| w.iter().zip(substr.els.iter()).all(|(x1, x2)| *x1 == *x2))
    }
}
impl<'a> FindToken<PreExpr> for Seq<'a> {
    fn find_token(&self, token: PreExpr) -> bool {
        self.iter_elements().any(|x| *x == token)
    }
}
impl<'a> InputIter for Seq<'a> {
    type Item = &'a PreExpr;
    type IterElem = std::iter::Take<std::iter::Skip<std::slice::Iter<'a, PreExpr>>>;
    type Iter = std::iter::Enumerate<Self::IterElem>;

    fn iter_elements(&self) -> Self::IterElem {
        self.els.iter().skip(self.range.start).take(self.len())
    }
    fn iter_indices(&self) -> Self::Iter {
        self.iter_elements().enumerate()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.iter_elements().position(predicate)
    }
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.len() >= count {
            Ok(count)
        } else {
            Err(nom::Needed::new(count - self.len()))
        }
    }
}
impl<'a> InputLength for Seq<'a> {
    fn input_len(&self) -> usize {
        self.len()
    }
}
impl<'a> InputTake for Seq<'a> {
    fn take(&self, count: usize) -> Self {
        use std::ops::Range;
        if count > self.len() {
            panic!()
        }
        Seq {
            els: self.els,
            range: Range {
                start: self.range.start,
                end: self.range.start + count,
            },
        }
    }
    fn take_split(&self, count: usize) -> (Self, Self) {
        use std::ops::Range;
        if count > self.len() {
            panic!()
        }
        (
            Seq {
                // suffix
                els: self.els,
                range: Range {
                    start: self.range.start + count,
                    end: self.range.end,
                },
            },
            Seq {
                // prefix
                els: self.els,
                range: Range {
                    start: self.range.start,
                    end: self.range.start + count,
                },
            },
        )
    }
}
impl<'a> InputTakeAtPosition for Seq<'a> {
    type Item = &'a PreExpr;
    fn split_at_position<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        Ok(self.take_split(
            self.position(predicate)
                .ok_or(nom::Err::Incomplete(nom::Needed::new(1)))?,
        ))
    }
    fn split_at_position1<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }
    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }
    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position1(predicate, e) {
            Err(nom::Err::Incomplete(_)) => {
                if self.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
            res => res,
        }
    }
}
impl<'a> Offset for Seq<'a> {
    fn offset(&self, second: &Self) -> usize {
        second.range.start - self.range.start
    }
}
impl<'a> ParseTo<PreExpr> for Seq<'a> {
    fn parse_to(&self) -> Option<PreExpr> {
        Some(PreExpr::Parens(self.els.clone()))
    }
}
impl<'a, R: RangeBounds<usize>> Slice<R> for Seq<'a> {
    fn slice(&self, range: R) -> Self {
        use std::ops::Bound::*;
        let start = self.range.start
            + match range.start_bound() {
                Included(x) => *x,
                Excluded(x) => *x + 1,
                Unbounded => 0,
            };
        let end = self.range.start
            + match range.end_bound() {
                Included(x) => *x + 1,
                Excluded(x) => *x,
                Unbounded => self.len(),
            };
        Seq {
            els: self.els,
            range: start..end,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum PreExpr {
    Id(String),
    Literal(Literal),
    Op(Op),
    Parens(Vec<PreExpr>),
}
impl PreExpr {
    fn parse_id(i: &str) -> IResult<&str, PreExpr, VerboseError<&str>> {
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s| PreExpr::Id(String::from(s)),
        )(i)
    }
    fn parse_parens(i: &str) -> IResult<&str, PreExpr, VerboseError<&str>> {
        delimited(
            terminated(tag("("), multispace0),
            map(PreExpr::parse_seq, PreExpr::Parens),
            preceded(multispace0, tag(")")),
        )(i)
    }
    fn parse_atomic(i: &str) -> IResult<&str, PreExpr, VerboseError<&str>> {
        alt((
            map(Literal::parse, PreExpr::Literal),
            PreExpr::parse_parens,
            PreExpr::parse_id,
            map(Op::parse, PreExpr::Op),
        ))(i)
    }
    fn parse_seq(i: &str) -> IResult<&str, Vec<PreExpr>, VerboseError<&str>> {
        many1(terminated(PreExpr::parse_atomic, multispace0))(i)
    }
}
impl AsChar for PreExpr {
    fn as_char(self) -> char {
        Default::default()
    }

    fn is_alpha(self) -> bool {
        false
    }

    fn is_alphanum(self) -> bool {
        false
    }

    fn is_dec_digit(self) -> bool {
        false
    }

    fn is_hex_digit(self) -> bool {
        false
    }

    fn is_oct_digit(self) -> bool {
        false
    }

    fn len(self) -> usize {
        1
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Id(String),
    Literal(Literal),
    BinOp(Box<Expr>, Op, Box<Expr>),
    UnOp(Op, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}
#[derive(Debug, PartialEq)]
enum ParseError<I> {
    // nom Error
    E(nom::error::Error<I>),
    // nom VerboseError
    V(VerboseError<I>),
    // custom error
    C,
}
impl<I: Clone> nom::error::ParseError<I> for ParseError<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self::E(nom::error::Error { input, code: kind })
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        match other {
            Self::C => Self::C,
            Self::E(e) => Self::E(nom::error::Error {
                input: e.input,
                code: e.code,
            }),
            Self::V(e) => Self::V(VerboseError {
                errors: {
                    let mut new_errors = e.errors.clone();
                    new_errors.push((input, nom::error::VerboseErrorKind::Nom(kind)));
                    new_errors
                },
            }),
        }
    }
}
impl<I, E> FromExternalError<I, E> for ParseError<I> {
    fn from_external_error(input: I, kind: nom::error::ErrorKind, _e: E) -> Self {
        Self::E(nom::error::Error { input, code: kind })
    }
}

impl Expr {
    fn anypre(i: Seq) -> IResult<Seq, &PreExpr, ParseError<Seq>> {
        if let Some(p) = i.iter_elements().next() {
            Ok((i.slice(1..), p))
        } else {
            Err(nom::Err::Error(ParseError::C))
        }
    }
    fn parse_raw_parens<'a>(i: Seq<'a>) -> IResult<Seq, &'a Vec<PreExpr>, ParseError<Seq>> {
        map_res(Expr::anypre, |p| {
            if let PreExpr::Parens(v) = p {
                Ok(v)
            } else {
                Err(())
            }
        })(i)
    }
    fn parse_pre(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        let (rest, p) = Expr::anypre(i)?;
        let expr = match p {
            PreExpr::Id(s) => Expr::Id(s.to_string()),
            PreExpr::Literal(l) => Expr::Literal(l.clone()),
            PreExpr::Op(_) => Err(nom::Err::Error(ParseError::C))?,
            PreExpr::Parens(v) => match Expr::parse(Seq::new(&v)) {
                Ok((_, expr)) => expr,
                Err(_) => Err(nom::Err::Error(ParseError::C))?,
            },
        };
        Ok((rest, expr))
    }
    fn parse_id(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        map_res(Expr::anypre, |e| {
            if let PreExpr::Id(s) = e {
                Ok(Expr::Id(s.clone()))
            } else {
                Err(())
            }
        })(i)
    }
    fn parse_literal(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        map_res(Expr::anypre, |e| {
            if let PreExpr::Literal(s) = e {
                Ok(Expr::Literal(s.clone()))
            } else {
                Err(())
            }
        })(i)
    }
    fn parse_raw_op(i: Seq) -> IResult<Seq, Op, ParseError<Seq>> {
        map_res(Expr::anypre, |p| {
            if let PreExpr::Op(o) = p {
                Ok(o.clone())
            } else {
                Err(())
            }
        })(i)
    }
    fn tag(p: &PreExpr) -> impl Parser<Seq, &PreExpr, ParseError<Seq>> {
        verify(Expr::anypre, |other: &PreExpr| *other == *p)
    }
    fn parse_raw_tuple(i: Seq) -> IResult<Seq, Vec<Expr>, ParseError<Seq>> {
        let (rest, v) = Expr::parse_raw_parens(i)?;
        let (inner_rest, els) =
            separated_list1(Expr::tag(&PreExpr::Op(Op::Comma)), Expr::parse_pre)(Seq::new(v))?;
        if inner_rest.len() == 0 {
            // Only accept a tuple that is a proper separated list occupying an entire parens expression
            Ok((rest, els))
        } else {
            Err(nom::Err::Failure(ParseError::C))
        }
    }
    fn parse_call(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        map(
            pair(Expr::parse_id, many1(Expr::parse_raw_tuple)),
            |(fname, calls)| {
                calls
                    .into_iter()
                    .fold(fname, |acc, c| Expr::Call(Box::new(acc), c))
            },
        )(i)
    }
    fn parse_unop(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        map(
            pair(Expr::parse_raw_op, Expr::parse_atomic),
            |(unop, atomic)| Expr::UnOp(unop, Box::new(atomic)),
        )(i)
    }
    fn parse_atomic(i: Seq) -> IResult<Seq, Expr, ParseError<Seq>> {
        alt((Expr::parse_call, Expr::parse_unop, Expr::parse_pre))(i)
    }
    fn parse_binop_seq(i: Seq) -> IResult<Seq, (Vec<(Expr, Op)>, Expr), ParseError<Seq>> {
        pair(
            many0(pair(Expr::parse_atomic, Expr::parse_raw_op)),
            Expr::parse_atomic,
        )(i)
    }
    fn order_binops((mut v, mut last): (Vec<(Expr, Op)>, Expr)) -> Expr {
        loop {
            let (max_idx, _) = match v
                .iter()
                .enumerate()
                .max_by(|(_, (_, o1)), (_, (_, o2))| o1.cmp(o2))
            {
                None => {
                    return last; // Escape from the loop
                }
                Some(x) => x,
            };

            let (max_expr, max_op) = v.remove(max_idx);
            if max_idx == v.len() {
                last = Expr::BinOp(Box::new(max_expr), max_op, Box::new(last));
            } else {
                let (next_expr, next_op) = v.remove(max_idx);
                v.insert(
                    max_idx,
                    (
                        Expr::BinOp(Box::new(max_expr), max_op, Box::new(next_expr)),
                        next_op,
                    ),
                )
            }
        }
    }
}
impl<'a> Parse<Seq<'a>, ParseError<Seq<'a>>> for Expr {
    fn parse(i: Seq) -> IResult<Seq, Self, ParseError<Seq>> {
        map(Expr::parse_binop_seq, Expr::order_binops)(i)
    }
}

fn main() {
    let test_str = r#"5 * -2 + cos(0) / fn(a, 2, "yes \"please\"")"#;
    match PreExpr::parse_seq(test_str) {
        Ok((_rest, m)) => {
            match Expr::parse(Seq::new(&m)) {
                Ok((_rest, m)) => println!("{:?}", m),
                Err(e) => println!("{:?}", e),
            };
        }
        Err(e) => println!("{:?}", e),
    }
}
