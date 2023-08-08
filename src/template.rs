use std::collections::HashMap;

use nom::{
    self,
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{all_consuming, cut, map, map_res, opt, recognize, value},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    FindSubstring, IResult, InputTake, Parser,
};

// Template engine!
// delimiter default: {{}}
// expressions:
// ns1.ns2.ns3.name
// expr ? exprtrue : exprfalse
// ! expr

trait Parse<I: nom::InputLength>
where
    Self: Sized,
{
    fn parse(i: I) -> IResult<I, Self>;

    fn parse_all(i: I) -> IResult<I, Self> {
        all_consuming(Self::parse)(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Literal {
    Str(String),
    Bool(bool),
    List(Vec<Literal>),
}
impl Literal {
    fn render_string(&self) -> String {
        // Produce an "in-language" display rendering
        // Eg. Str("Hello") -> Hello
        // not Str("Hello") -> "Hello"
        use Literal::*;
        match self {
            Str(s) => s.clone(),
            Bool(b) => (if *b { "true" } else { "false" }).to_string(),
            List(v) => v.iter().fold(String::new(), |mut acc, x| {
                acc.push_str(&x.render_string());
                acc
            }),
        }
    }
    fn type_str(&self) -> &'static str {
        use Literal::*;
        match self {
            Str(_) => "str",
            Bool(_) => "bool",
            List(_) => "list",
        }
    }
    fn get_bool(&self) -> Result<bool, EvalError> {
        match self {
            Literal::Bool(b) => Ok(*b),
            l => Err(EvalError::ExpectedBool(l.clone())),
        }
    }
    fn get_default(&self) -> Option<Literal> {
        match self {
            Literal::Str(_) => Some(Literal::Str(String::new())),
            _ => None,
        }
    }
    fn parse_str(i: &str) -> IResult<&str, Literal> {
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
                cut(tag("\"")),
            ),
            Literal::Str,
        )(i)
    }
    fn parse_bool(i: &str) -> IResult<&str, Literal> {
        alt((
            value(Literal::Bool(true), tag("true")),
            value(Literal::Bool(false), tag("false")),
        ))(i)
    }
}
impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Literal::*;
        match self {
            Str(s) => write!(f, "{:?}", s),
            Bool(b) => write!(f, "{:?}", b),
            List(v) => write!(f, "{:?}", v),
        }
    }
}
impl Parse<&str> for Literal {
    fn parse(i: &str) -> IResult<&str, Literal> {
        alt((Literal::parse_bool, Literal::parse_str))(i)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum EvalError {
    NameDNE(String),
    NotNS(Literal, String),
    ExpectedBool(Literal),
    ExpectedStr(Literal),
    NoDefault(Literal),
}
impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use EvalError::*;
        match self {
            NameDNE(s) => write!(
                f,
                "the name \"{}\" does not exist in the relevant namespace",
                s
            ),
            NotNS(l, s) => write!(
                f,
                "cannot access field {} of {}: {} is a {}, not a namespace",
                s,
                l,
                l,
                l.type_str()
            ),
            ExpectedBool(l) => write!(f, "expected bool, got {} {}", l.type_str(), l),
            ExpectedStr(l) => write!(f, "expected str, got {} {}", l.type_str(), l),
            NoDefault(l) => write!(
                f,
                "conditional has no false branch but {} {} has no default value",
                l.type_str(),
                l
            ),
        }
    }
}

enum NSElement {
    NS(HashMap<String, NSElement>),
    Val(Literal),
}
type Namespace = HashMap<String, NSElement>;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expr {
    // Atomic
    Literal(Literal),
    Id(Vec<String>),
    Not(Box<Expr>),
    List(Vec<Expr>),
    // Non-atomic
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Concat(Vec<Expr>),
}
impl Expr {
    fn parse_literal(i: &str) -> IResult<&str, Expr> {
        map(Literal::parse, Expr::Literal)(i)
    }
    fn parse_name(i: &str) -> IResult<&str, &str> {
        recognize(tuple((alpha1, many0(alt((alphanumeric1, tag("_")))))))(i)
    }
    fn parse_id(i: &str) -> IResult<&str, Expr> {
        map(
            separated_list1(tag("."), map(Expr::parse_name, String::from)),
            Expr::Id,
        )(i)
    }
    fn parse_not(i: &str) -> IResult<&str, Expr> {
        map(
            preceded(terminated(tag("!"), multispace0), cut(Expr::parse_atomic)),
            |x| Expr::Not(Box::new(x)),
        )(i)
    }
    fn parse_parens(i: &str) -> IResult<&str, Expr> {
        delimited(
            terminated(tag("("), multispace0),
            Expr::parse,
            preceded(multispace0, cut(tag(")"))),
        )(i)
    }
    fn parse_list(i: &str) -> IResult<&str, Expr> {
        map(
            delimited(
                pair(char('['), multispace0),
                separated_list0(delimited(multispace0, char(','), multispace0), Expr::parse),
                tuple((multispace0, opt(pair(char(','), multispace0)), char(']'))),
            ),
            Expr::List,
        )(i)
    }
    fn parse_atomic(i: &str) -> IResult<&str, Expr> {
        alt((
            // Mutually exclusive atomic parsers
            Expr::parse_literal,
            Expr::parse_id,
            Expr::parse_not,
            Expr::parse_parens,
            Expr::parse_list,
        ))(i)
    }
    fn parse_if_remainder(i: &str) -> IResult<&str, (Expr, Option<Expr>)> {
        let (rest, _) = delimited(
            multispace0,
            tag::<&str, &str, nom::error::Error<&str>>("?"),
            multispace0,
        )(i)?;
        let (rest, (t, f)) = cut(tuple((
            Expr::parse_atomic,
            opt(preceded(
                delimited(multispace0, tag(":"), multispace0),
                cut(Expr::parse_atomic),
            )),
        )))(rest)?;
        Ok((rest, (t, f)))
    }
    fn parse_concat_remainder(i: &str) -> IResult<&str, Vec<Expr>> {
        many1(preceded(
            delimited(multispace0, tag("+"), multispace0),
            cut(Expr::parse_atomic),
        ))(i)
    }
    fn no_fail<T, E>(r: IResult<&str, T, E>) -> Result<Option<(&str, T)>, nom::Err<E>> {
        use nom::Err::*;
        match r {
            Ok(x) => Ok(Some(x)),
            Err(e) => match e {
                Incomplete(_) => Ok(None),
                Error(_) => Ok(None),
                Failure(e) => Err(Failure(e)),
            },
        }
    }
}

trait Eval<O, NS, E> {
    fn eval(&self, namespace: &NS) -> Result<O, E>;
}

impl Eval<Literal, Namespace, EvalError> for Expr {
    fn eval(&self, namespace: &HashMap<String, NSElement>) -> Result<Literal, EvalError> {
        match self {
            Expr::Literal(l) => Ok(l.clone()),
            Expr::Id(names) => {
                let mut ns = namespace;
                let mut idx = 0;
                loop {
                    match ns.get(&names[idx]) {
                        None => Err(EvalError::NameDNE(names[idx].clone()))?,
                        Some(el) => match el {
                            NSElement::Val(l) => {
                                if idx == names.len() - 1 {
                                    return Ok(l.clone());
                                } else {
                                    return Err(EvalError::NotNS(
                                        l.clone(),
                                        names[idx + 1].clone(),
                                    ));
                                }
                            }
                            NSElement::NS(new_ns) => {
                                ns = new_ns;
                            }
                        },
                    };
                    idx += 1;
                }
            }
            Expr::Not(x) => x.eval(namespace)?.get_bool().map(|b| Literal::Bool(!b)),
            Expr::If(predicate, t, f) => {
                if predicate.eval(namespace)?.get_bool()? {
                    t.eval(namespace)
                } else {
                    match f {
                        Some(x) => x.eval(namespace),
                        None => {
                            let t = t.eval(namespace)?;
                            t.get_default().ok_or(EvalError::NoDefault(t.clone()))
                        }
                    }
                }
            }
            Expr::Concat(atoms) => atoms
                .iter()
                .try_fold(String::new(), |mut acc, x| {
                    let l = x.eval(namespace)?;
                    match l {
                        Literal::Str(s) => {
                            acc.extend(s.chars());
                            Ok(acc)
                        }
                        _ => Err(EvalError::ExpectedStr(l.clone())),
                    }
                })
                .map(|s| Literal::Str(s)),
            Expr::List(v) => Ok(Literal::List(
                v.iter()
                    .map(|x| x.eval(namespace))
                    .collect::<Result<Vec<Literal>, _>>()?,
            )),
        }
    }
}

impl Parse<&str> for Expr {
    fn parse(i: &str) -> IResult<&str, Self> {
        let (rest, first_atomic) = Expr::parse_atomic(i)?;
        if let Some((rest, (t, f))) = Expr::no_fail(Expr::parse_if_remainder(rest))? {
            Ok((
                rest,
                Expr::If(Box::new(first_atomic), Box::new(t), f.map(Box::new)),
            ))
        } else if let Some((rest, mut v)) = Expr::no_fail(Expr::parse_concat_remainder(rest))? {
            v.insert(0, first_atomic);
            Ok((rest, Expr::Concat(v)))
        } else {
            Ok((rest, first_atomic))
        }
    }
}

fn take_until_opt1<'a>(
    tag: &'static str,
) -> impl Parser<&'a str, &'a str, nom::error::Error<&'a str>> {
    |i: &'a str| {
        let idx = i.find_substring(tag).unwrap_or(i.len());
        if idx == 0 {
            Err(nom::Err::Error(nom::error::Error {
                input: i,
                code: nom::error::ErrorKind::TakeUntil,
            }))
        } else {
            Ok(i.take_split(idx))
        }
    }
}

fn template_transform<'a>(
    i: &'a str,
    namespace: &Namespace,
) -> Result<String, nom::Err<nom::error::Error<&'a str>>> {
    enum StrLike<'a> {
        Str(String),
        Slice(&'a str),
    }
    use StrLike::*;
    Ok(all_consuming(many0(alt((
        map_res(delimited(tag("{{"), Expr::parse, cut(tag("}}"))), |x| {
            Ok::<_, EvalError>(Str(x.eval(namespace)?.render_string()))
        }),
        map(take_until_opt1("{{"), Slice),
    ))))(i)?
    .1
    .iter()
    .fold(String::new(), |mut acc, x| match x {
        Str(s) => {
            acc.push_str(s);
            acc
        }
        Slice(s) => {
            acc.push_str(s);
            acc
        }
    }))
}

fn parse_ns_id(i: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0(alt((tag("_"), alphanumeric1)))))(i)
}
fn parse_ns_el(i: &str) -> IResult<&str, NSElement> {
    alt((
        map_res(Expr::parse, |x| x.eval(&HashMap::new()).map(NSElement::Val)),
        map(parse_ns, NSElement::NS),
    ))(i)
}
fn parse_ns(i: &str) -> IResult<&str, HashMap<String, NSElement>> {
    map(
        delimited(
            delimited(multispace0, tag("{"), multispace0),
            separated_list0(
                delimited(multispace0, char(','), multispace0),
                pair(
                    terminated(map(parse_ns_id, String::from), pair(multispace0, char(':'))),
                    preceded(multispace0, parse_ns_el),
                ),
            ),
            preceded(
                opt(preceded(multispace0, char(','))),
                preceded(multispace0, cut(tag("}"))),
            ),
        ),
        |v| v.into_iter().collect(),
    )(i)
}

fn main() {
    test_template();
}

fn test_template() {
    let template = r#"hello!
this is a {{"template"}}!
it is {{(true ? "easy" : "hard") + " to use"}}
the narrator is{{!narrator.excited? " not"}} excited to tell you that {{narrator.male ? "his" : "her"}} favorite words are [{{narrator.favorite_words}}]. {{narrator.male? "He" : "She"}} also has dogs {{narrator.dogs}}
template regions begin with "{{"{{"}}" and end with "{{"}}"}}"
"#;
    let ns = match parse_ns(
        r#"{
        narrator: {
            favorite_words: ["true", "false"],
            dogs: [
                {
                    name: "Martha",
                },
                {
                    name: "Bartha",
                },
            ],
            excited: true,
            male: true,
        }
    }"#,
    ) {
        Ok(x) => x.1,
        Err(e) => {
            println!("{:?}", e);
            HashMap::new()
        }
    };

    match template_transform(template, &ns) {
        Ok(s) => println!("{}", s),
        Err(e) => println!("{:?}", e),
    };
}

fn _test_parse() {
    let test_ns = HashMap::from([(
        "narrator".to_string(),
        NSElement::NS(HashMap::from([(
            "excited".to_string(),
            NSElement::Val(Literal::Bool(false)),
        )])),
    )]);
    let test_str = r#""hello, world" + (narrator.excited ? "!" : ".")"#;
    match Expr::parse_all(test_str) {
        Ok((_rest, m)) => {
            println!("{:?}", m);
            match m.eval(&test_ns) {
                Ok(l) => println!("{:?}", l),
                Err(e) => println!("EvalError: {}", e),
            }
        }
        Err(e) => println!("{:?}", e),
    }
}
