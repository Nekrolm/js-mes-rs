use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    combinator::map_res,
    multi::many0,
    sequence::{delimited, pair},
    Parser,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Var,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Assign,
    Plus,
}

#[derive(Debug, Copy, Clone)]
pub enum NumberValue {
    Integer(u64),
    Double(f64),
}

#[derive(Debug, Copy, Clone)]
pub enum TokenValue<'a> {
    None,
    Number(NumberValue),
    String(&'a str),
}

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    kind: TokenKind,
    value: TokenValue<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    BinaryOp(BinaryOp),
    Identifier,
    Number,
}

fn spaces(input: &str) -> nom::IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace()).parse(input)
}

pub fn tokenize(input: &str) -> nom::IResult<&str, Vec<Token>> {
    let tokens = alt((keyword, identifier, binaryop, number));

    let token_with_spaces = delimited(spaces, tokens, spaces);
    let mut tokens = many0(token_with_spaces);
    tokens(input)
}

fn keyword(input: &str) -> nom::IResult<&str, Token> {
    nom::bytes::complete::tag("var")
        .map(|_: &str| Keyword::Var)
        .map(|kv| Token {
            kind: TokenKind::Keyword(kv),
            value: TokenValue::None,
        })
        .parse(input)
}

unsafe fn concat_unchecked<'a>(lhs: &'a str, rhs: &'a str) -> &'a str {
    assert!(lhs.as_ptr().add(lhs.len()) == rhs.as_ptr());
    std::str::from_utf8_unchecked(std::slice::from_raw_parts(
        lhs.as_ptr(),
        lhs.len() + rhs.len(),
    ))
}

fn identifier(input: &str) -> nom::IResult<&str, Token> {
    let prefixed_ident = pair(
        tag("_"),
        take_while(|x: char| x.is_alphanumeric() || x == '_'),
    );

    let ident = pair(
        take_while1(|x: char| x.is_alphabetic()),
        take_while(|x: char| x.is_alphanumeric() || x == '_'),
    );

    alt((prefixed_ident, ident))
        .map(|(ident_prefix, ident_suffix)| Token {
            kind: TokenKind::Identifier,
            value: TokenValue::String(unsafe { concat_unchecked(ident_prefix, ident_suffix) }),
        })
        .parse(input)
}

fn binaryop(input: &str) -> nom::IResult<&str, Token> {
    let plus = tag("+").map(|_: &str| BinaryOp::Plus);
    let assign = tag("=").map(|_: &str| BinaryOp::Assign);

    alt((plus, assign))
        .map(|op| Token {
            kind: TokenKind::BinaryOp(op),
            value: TokenValue::None,
        })
        .parse(input)
}

fn integer(input: &str) -> nom::IResult<&str, Token> {
    let digits = take_while(|x: char| x.is_ascii_digit());
    map_res(digits, |digits: &str| digits.parse::<u64>())
        .map(|int: u64| Token {
            kind: TokenKind::Number,
            value: TokenValue::Number(NumberValue::Integer(int)),
        })
        .parse(input)
}

fn number(input: &str) -> nom::IResult<&str, Token> {
    let double = nom::number::complete::double.map(|val| Token {
        kind: TokenKind::Number,
        value: TokenValue::Number(NumberValue::Double(val)),
    });

    alt((integer, double)).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier() {
        let ident = "_122345";
        let (rest, parsed) = identifier(ident).expect("ok");
        assert_eq!(rest, "");
        assert_eq!(parsed.kind, TokenKind::Identifier);
        assert!(matches!(parsed.value, TokenValue::String("_122345")));
    }

    #[test]
    fn test_parse_js() {
        let script = r#"
          var sdfвввввsdsd    = 12345
          var ___ass_ = abcd
        "#;

        let (rest, parsed) = tokenize(script).expect("valid");

        println!("{parsed:?}");
        assert_eq!(rest, "");
    }
}
