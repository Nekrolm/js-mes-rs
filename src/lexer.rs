use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    combinator::{cut, map_res},
    multi::{fold_many0},
    sequence::{delimited, pair, preceded},
    Parser,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Var,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SpecialSymbol {
    Assign,
    Plus,
    Minus,
    Colon,
    Comma,
    Star,
    Slash,
    Semicolon,
    Dot,
    LeftParen,
    RightParen,
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
    pub kind: TokenKind,
    pub value: TokenValue<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Comment,
    Keyword(Keyword),
    SpecialSymbol(SpecialSymbol),
    Identifier,
    Literal,
}

pub fn tokenize(input: &str) -> nom::IResult<&str, Vec<Token>> {
    let tokens = alt((keyword, identifier, comment, special_symbol, number, string_literal));

    let token_with_spaces = delimited(spaces, tokens, spaces);
    let mut tokens = fold_many0(token_with_spaces, Vec::default, |mut acc, token| {
        if token.kind != TokenKind::Comment {
            acc.push(token)
        }
        acc
    });
    tokens(input)
}

fn spaces(input: &str) -> nom::IResult<&str, &str> {
    take_while(|c: char| c.is_whitespace()).parse(input)
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
        cut(take_while(|x: char| x.is_alphanumeric() || x == '_')),
    );

    let ident = pair(
        take_while1(|x: char| x.is_alphabetic()),
        cut(take_while(|x: char| x.is_alphanumeric() || x == '_')),
    );

    alt((prefixed_ident, ident))
        .map(|(ident_prefix, ident_suffix)| Token {
            kind: TokenKind::Identifier,
            value: TokenValue::String(unsafe { concat_unchecked(ident_prefix, ident_suffix) }),
        })
        .parse(input)
}

fn special_symbol(input: &str) -> nom::IResult<&str, Token> {
    let mut chars = input.chars();
    let ch = chars.next();
    let rest = chars.as_str();

    let ch = ch.unwrap_or('\0');

    let symbol = match ch {
        '+' => SpecialSymbol::Plus,
        '-' => SpecialSymbol::Minus,
        '=' => SpecialSymbol::Assign,
        '*' => SpecialSymbol::Star,
        '/' => SpecialSymbol::Slash,
        ':' => SpecialSymbol::Colon,
        ',' => SpecialSymbol::Comma,
        ';' => SpecialSymbol::Semicolon,
        '.' => SpecialSymbol::Dot,
        '(' => SpecialSymbol::LeftParen,
        ')' => SpecialSymbol::RightParen,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::OneOf,
            )))
        }
    };

    Ok((
        rest,
        Token {
            kind: TokenKind::SpecialSymbol(symbol),
            value: TokenValue::None,
        },
    ))
}

fn integer(input: &str) -> nom::IResult<&str, Token> {
    let digits = take_while(|x: char| x.is_ascii_digit());
    map_res(digits, |digits: &str| digits.parse::<u64>())
        .map(|int: u64| Token {
            kind: TokenKind::Literal,
            value: TokenValue::Number(NumberValue::Integer(int)),
        })
        .parse(input)
}

fn number(input: &str) -> nom::IResult<&str, Token> {
    let double = nom::number::complete::double.map(|val| Token {
        kind: TokenKind::Literal,
        value: TokenValue::Number(NumberValue::Double(val)),
    });

    alt((integer, double)).parse(input)
}

fn string_literal(input: &str) -> nom::IResult<&str, Token> {
    let string_payload = fold_many0(
        alt((
            tag(r#"\""#),                                               // try eagerly consume \"
            tag(r#"\"#), // if failed, try to consume \
            take_while1(|c: char| c != '\n' && c != '\"' && c != '\\'), // consume any other characters except newline
        )),
        || "",
        |acc: &str, item: &str| {
            if acc.is_empty() {
                item
            } else {
                unsafe { concat_unchecked(acc, item) }
            }
        },
    );

    delimited(tag("\""), string_payload, tag("\""))
        .map(|s: &str| Token {
            kind: TokenKind::Literal,
            value: TokenValue::String(s),
        })
        .parse(input)
}

fn comment(input: &str) -> nom::IResult<&str, Token> {
    preceded(tag("//"), take_while(|x: char| x != '\n'))
        .map(|comment: &str| Token {
            kind: TokenKind::Comment,
            value: TokenValue::String(comment),
        })
        .parse(input)
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
          var s = "  11 11 \" // this is not comment"
          // comment
               // another comment
        "#;

        let (rest, parsed) = tokenize(script).expect("valid");

        println!("{parsed:?}");
        assert_eq!(rest, "");
    }

    #[test]
    fn test_parse_string_literal() {
        let s = r#"" \' \' 11 11 \" ""#;
        let (rest, token) = string_literal(s).expect("parsed");
        println!("{token:?}");
        assert_eq!(rest, "");
    }
}
