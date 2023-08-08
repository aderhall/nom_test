use nom::{error::Error, IResult};

//fn parens(input: &str) -> IResult<&str, &str> {
//    delimited(char('('), is_not(")"), char(')'))(input)
//}

#[derive(Debug)]
struct PhoneNumber<T> {
    country: T,
    area: [T; 3],
    digits: [T; 7],
}

fn decimal_digits<'a, 'b>(
    n: usize,
    ignore: &'b str,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<u8>> + 'b {
    use nom::{
        character::complete::one_of,
        combinator::recognize,
        multi::{count, many0},
        sequence::preceded,
    };
    let parser = move |input: &'a str| {
        let (rest, digit_string) = recognize(count(
            preceded(many0(one_of(ignore)), one_of("0123456789")),
            n,
        ))(input)?;
        Ok((
            rest,
            digit_string
                .chars()
                .filter_map(|c| c.to_digit(10).and_then(|i| Some(i as u8)))
                .collect(),
        ))
    };

    return parser;
}

//fn decimal_digits<'a>(input: &'a str, n: usize, ignore: &str) -> IResult<&'a str, Vec<u8>> {
//    use nom::{
//        character::complete::one_of,
//        combinator::recognize,
//        multi::{count, many0},
//        sequence::terminated,
//    };
//    let (rest, digit_string) = recognize(count(
//        terminated(one_of("0123456789"), many0(one_of(ignore))),
//        n,
//    ))(input)?;
//    Ok((
//        rest,
//        digit_string
//            .chars()
//            .map(|c| c.to_digit(10).unwrap() as u8)
//            .collect(),
//    ))
//    //map_res(recognize(one_of("0123456789")), |s| {
//    //    u8::from_str_radix(s, 10)
//    //})(input)
//}

fn area_code(input: &str) -> IResult<&str, Vec<u8>> {
    use nom::{bytes::complete::tag, sequence::delimited};
    delimited(tag("("), decimal_digits(3, ""), tag(")"))(input)
}

fn phone_num(input: &str) -> IResult<&str, PhoneNumber<u8>> {
    let (rest, area) = area_code(input)?;
    let (rest, digits) = decimal_digits(7, " -")(rest)?;
    Ok((
        rest,
        PhoneNumber {
            country: 1,
            area: area.try_into().unwrap(),
            digits: digits.try_into().unwrap(),
        },
    ))
}

fn main() {
    let test_str = "(875) 432-1111";
    match phone_num(test_str) {
        Ok((rest, matched)) => println!("Matched {:?}, Rest = {:?}", matched, rest),
        Err(e) => println!("{:?}", e),
    }
}
