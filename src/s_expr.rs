use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, digit1, multispace0, multispace1, one_of},
    combinator::{cut, map, map_res, opt, recognize},
    error::{context, VerboseError},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum BuiltIn {
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Not,
}
impl std::fmt::Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BuiltIn::*;
        write!(
            f,
            "{}",
            match self {
                Plus => "+",
                Minus => "-",
                Times => "*",
                Divide => "/",
                Equal => "=",
                Not => "not",
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Atom {
    Num(i32),
    Keyword(String),
    Boolean(bool),
    BuiltIn(BuiltIn),
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Expr {
    Constant(Atom),
    Application(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Quote(Vec<Expr>),
    Empty,
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Atom::*;
        use Expr::*;
        match self {
            Empty => write!(f, "()"),
            Constant(atom) => match atom {
                Num(x) => write!(f, "{}", x),
                Boolean(x) => write!(f, "{}", x),
                Keyword(x) => write!(f, ":{}", x),
                BuiltIn(x) => write!(f, "{}", x),
            },
            Application(head, tail) => write!(
                f,
                "({} {})",
                head,
                tail.iter().fold(String::new(), |mut acc, x| {
                    acc.push_str(&format!(" {}", x));
                    acc
                })
            ),
            If(condition, pos, neg) => match neg {
                Some(n) => write!(f, "(if {} {} {})", condition, pos, n),
                None => write!(f, "(if {} {})", condition, pos),
            },
            Quote(contents) => write!(
                f,
                "'{}",
                contents.iter().fold(String::new(), |mut acc, x| {
                    acc.push_str(&format!(" {}", x));
                    acc
                })
            ),
        }
    }
}

fn parse_builtin_op(i: &str) -> IResult<&str, BuiltIn, VerboseError<&str>> {
    let (i, t) = one_of("+-*/=")(i)?;

    Ok((
        i,
        match t {
            '+' => BuiltIn::Plus,
            '-' => BuiltIn::Minus,
            '*' => BuiltIn::Times,
            '/' => BuiltIn::Divide,
            '=' => BuiltIn::Equal,
            _ => unreachable!(),
        },
    ))
}

fn parse_builtin(i: &str) -> IResult<&str, BuiltIn, VerboseError<&str>> {
    alt((parse_builtin_op, map(tag("not"), |_| BuiltIn::Not)))(i)
}

fn parse_bool(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    alt((
        map(tag("#t"), |_| Atom::Boolean(true)),
        map(tag("#f"), |_| Atom::Boolean(false)),
    ))(i)
}

fn parse_keyword(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    context(
        "keyword",
        map(preceded(tag(":"), cut(alpha1)), |name| {
            Atom::Keyword(String::from(name))
        }),
    )(i)
}

fn parse_num(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    map_res(alt((digit1, recognize(tag("-").and(digit1)))), |s| {
        i32::from_str_radix(s, 10).map(|x| Atom::Num(x))
    })(i)
}

fn parse_atom(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
    alt((
        map(parse_builtin, Atom::BuiltIn),
        parse_bool,
        parse_keyword,
        parse_num,
    ))(i)
}

fn parse_constant(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    map(parse_atom, Expr::Constant)(i)
}

fn s_exp<'a, O1, F>(inner: F) -> impl Parser<&'a str, O1, VerboseError<&'a str>>
where
    F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
    delimited(
        tag("("),
        preceded(multispace0, inner),
        context("closing paren", cut(preceded(multispace0, tag(")")))),
    )
}

fn parse_application(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    s_exp(map(
        tuple((parse_expr, many0(parse_expr))),
        |(head, tail)| Expr::Application(Box::new(head), tail),
    ))
    .parse(i)
}

fn parse_if(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    map(
        preceded(
            terminated(tag("if"), multispace1),
            tuple((parse_expr, parse_expr, opt(parse_expr))),
        ),
        |(condition, pos, neg)| Expr::If(Box::new(condition), Box::new(pos), neg.map(Box::new)),
    )(i)
}

fn parse_quote(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    context(
        "quote",
        map(
            preceded(tag("'"), cut(s_exp(many0(parse_expr)))),
            Expr::Quote,
        ),
    )(i)
}

fn parse_expr(i: &str) -> IResult<&str, Expr, VerboseError<&str>> {
    preceded(
        multispace0,
        alt((parse_constant, parse_if, parse_quote, parse_application)),
    )(i)
}

fn get_num_from_expr(e: &Expr) -> Option<i32> {
    if let Expr::Constant(Atom::Num(n)) = e {
        Some(*n)
    } else {
        None
    }
}

fn get_bool_from_expr(e: &Expr) -> Option<bool> {
    if let Expr::Constant(Atom::Boolean(b)) = e {
        Some(*b)
    } else {
        None
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum ErrorCode {
    ExpectedBool,
    ExpectedNum,
    ExpectedFunction,
    TooManyArgs,
    ZeroDiv,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct EvalError {
    code: ErrorCode,
    src: Expr,
    expl: Option<String>,
}
impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Atom::*;
        use ErrorCode::*;
        let instead = match &self.src {
            Expr::Constant(atom) => match atom {
                Num(_) => "number",
                Boolean(_) => "boolean",
                Keyword(_) => "keyword",
                BuiltIn(_) => "built-in function",
            },
            _ => "expression",
        };
        match self.code {
            ExpectedBool => write!(f, "Expected boolean, got {} {}", instead, self.src),
            ExpectedFunction => write!(f, "Expected function, got {} {}", instead, self.src),
            ExpectedNum => write!(f, "Expected number, got {} {}", instead, self.src),
            TooManyArgs => write!(
                f,
                "Got enough arguments and then got {} {}",
                instead, self.src
            ),
            ZeroDiv => write!(
                f,
                "Attempting to divide by {} {} results in division by zero",
                instead, self.src
            ),
        }
    }
}

fn eval_expression(e: Expr) -> Result<Expr, EvalError> {
    match e {
        Expr::Constant(_) | Expr::Quote(_) | Expr::Empty => Ok(e),
        Expr::If(condition, pos, neg) => {
            let reduced_condition = eval_expression(*condition)?;
            if get_bool_from_expr(&reduced_condition).ok_or(EvalError {
                code: ErrorCode::ExpectedBool,
                src: reduced_condition.clone(),
                expl: None,
            })? {
                eval_expression(*pos)
            } else {
                neg.map_or(Ok(Expr::Empty), |x| eval_expression(*x))
            }
        }
        Expr::Application(head, tail) => {
            let reduced_head = eval_expression(*head)?;
            let reduced_tail = tail
                .into_iter()
                .map(eval_expression)
                .collect::<Result<Vec<Expr>, EvalError>>()?;
            if reduced_tail.len() == 0 {
                return Ok(reduced_head);
            }
            // TODO: revert to if let, put the rest in a special else branch for when reduced_tail is empty?
            match &reduced_head {
                Expr::Constant(Atom::BuiltIn(bi)) => Ok(Expr::Constant(match bi {
                    BuiltIn::Plus => Atom::Num(
                        reduced_tail
                            .iter()
                            .map(get_num_from_expr)
                            .zip(&reduced_tail)
                            .try_fold(0, |acc, (x, src_expr)| {
                                x.map_or(
                                    Err(EvalError {
                                        code: ErrorCode::ExpectedNum,
                                        src: src_expr.clone(),
                                        expl: None,
                                    }),
                                    |i| Ok(acc + i),
                                )
                            })?,
                    ),
                    BuiltIn::Times => Atom::Num(
                        reduced_tail
                            .iter()
                            .map(get_num_from_expr)
                            .zip(&reduced_tail)
                            .try_fold(1, |acc, (x, src_expr)| {
                                x.map_or(
                                    Err(EvalError {
                                        code: ErrorCode::ExpectedNum,
                                        src: src_expr.clone(),
                                        expl: None,
                                    }),
                                    |i| Ok(acc * i),
                                )
                            })?,
                    ),
                    BuiltIn::Equal => Atom::Boolean(
                        reduced_tail
                            .iter()
                            .zip(reduced_tail.iter().skip(1))
                            .all(|(a, b)| a == b),
                    ),
                    BuiltIn::Not => Atom::Boolean(match reduced_tail.get(2) {
                        Some(extra) => Err(EvalError {
                            code: ErrorCode::TooManyArgs,
                            src: extra.clone(),
                            expl: None,
                        })?,
                        None => match reduced_tail.first() {
                            Some(arg) => !get_bool_from_expr(arg).ok_or(EvalError {
                                code: ErrorCode::ExpectedBool,
                                src: arg.clone(),
                                expl: None,
                            })?,
                            None => Err(EvalError {
                                code: ErrorCode::ExpectedBool,
                                src: Expr::Empty,
                                expl: None,
                            })?,
                        },
                    }),
                    BuiltIn::Minus => Atom::Num(if let Some(first_expr) = reduced_tail.first() {
                        let first_num = get_num_from_expr(first_expr).ok_or(EvalError {
                            code: ErrorCode::ExpectedNum,
                            src: first_expr.clone(),
                            expl: None,
                        })?;
                        reduced_tail
                            .iter()
                            .zip(&reduced_tail)
                            .skip(1)
                            .map(|(expr, src_expr)| (get_num_from_expr(expr), src_expr))
                            .try_fold(first_num, |acc, (x, src_expr)| {
                                x.map_or(
                                    Err(EvalError {
                                        code: ErrorCode::ExpectedNum,
                                        src: src_expr.clone(),
                                        expl: None,
                                    }),
                                    |i| Ok(acc - i),
                                )
                            })?
                    } else {
                        Err(EvalError {
                            code: ErrorCode::ExpectedNum,
                            src: Expr::Empty,
                            expl: None,
                        })?
                    }),
                    BuiltIn::Divide => Atom::Num(if let Some(first_expr) = reduced_tail.first() {
                        let first_num = get_num_from_expr(first_expr).ok_or(EvalError {
                            code: ErrorCode::ExpectedNum,
                            src: first_expr.clone(),
                            expl: None,
                        })?;
                        reduced_tail
                            .iter()
                            .zip(&reduced_tail)
                            .skip(1)
                            .map(|(expr, src_expr)| (get_num_from_expr(expr), src_expr))
                            .try_fold(first_num, |acc, (x, src_expr)| {
                                x.map_or(
                                    Err(EvalError {
                                        code: ErrorCode::ExpectedNum,
                                        src: src_expr.clone(),
                                        expl: None,
                                    }),
                                    |i| {
                                        if i != 0 {
                                            Ok(acc / i)
                                        } else {
                                            Err(EvalError {
                                                code: ErrorCode::ZeroDiv,
                                                src: src_expr.clone(),
                                                expl: None,
                                            })
                                        }
                                    },
                                )
                            })?
                    } else {
                        Err(EvalError {
                            code: ErrorCode::ExpectedNum,
                            src: Expr::Empty,
                            expl: None,
                        })?
                    }),
                })),
                Expr::Constant(atom @ Atom::Boolean(_)) | Expr::Constant(atom @ Atom::Num(_)) => {
                    // Application of a constant bool or num with no args just yields that bool or num
                    if reduced_tail.len() == 0 {
                        Ok(Expr::Constant(atom.clone()))
                    } else {
                        Err(EvalError {
                            code: ErrorCode::ExpectedFunction,
                            src: reduced_head.clone(),
                            expl: None,
                        })
                    }
                }
                _ => Err(EvalError {
                    code: ErrorCode::ExpectedFunction,
                    src: reduced_head,
                    expl: None,
                }),
            }
        }
    }
}

fn eval_from_str(i: &str) -> Result<Expr, String> {
    let (_, expr) = parse_expr(i).map_err(|e| format!("{:?}", e))?;
    eval_expression(expr).map_err(|e| format!("{}", e))
}

fn main() {
    let test_str = "(
        (if (
            = (+ 3 (/ 9 3)) (* 2 3)
        ) * /) 456 123
    )";

    match parse_expr(test_str) {
        Ok((_, m)) => println!("{}", m),
        Err(e) => println!("{:?}", e),
    }
    match eval_from_str(test_str) {
        Ok(expr) => println!("{}", expr),
        Err(e) => println!("{}", e),
    };
}
