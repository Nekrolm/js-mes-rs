use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    combinator::{cut, map_res},
    multi::fold_many0,
    sequence::{delimited, pair, preceded},
    Parser,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Var,
    Const,
    If,
    Else,
    While,
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
    Equals,
    Greater,
    Less,
    NotEquals,
    GreaterOrEq,
    LessOrEq,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    LeftBracket,
    RightBracket,
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

pub fn tokenize<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Token<'a>>> {
    let tokens = alt((
        keyword,
        identifier,
        comment,
        special_symbol,
        number,
        string_literal,
    ));

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
    alt((
        tag("var").map(|_: &str| Keyword::Var),
        tag("const").map(|_: &str| Keyword::Const),
        tag("if").map(|_: &str| Keyword::If),
        tag("else").map(|_: &str| Keyword::Else),
        tag("while").map(|_: &str| Keyword::While),
    ))
    .map(|kv| Token {
        kind: TokenKind::Keyword(kv),
        value: TokenValue::None,
    })
    .parse(input)
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

    let (rest, ident_len) = alt((prefixed_ident, ident))
        .map(|(ident_prefix, ident_suffix): (&str, &str)| ident_prefix.len() + ident_suffix.len())
        .parse(input)?;

    Ok((
        rest,
        Token {
            kind: TokenKind::Identifier,
            value: TokenValue::String(&input[..ident_len]),
        },
    ))
}

fn single_special_symbol(input: &str) -> nom::IResult<&str, SpecialSymbol> {
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
        '>' => SpecialSymbol::Greater,
        '<' => SpecialSymbol::Less,
        '!' => SpecialSymbol::LogicalNot,
        '{' => SpecialSymbol::LeftBracket,
        '}' => SpecialSymbol::RightBracket,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::OneOf,
            )))
        }
    };
    Ok((rest, symbol))
}

fn special_symbol(input: &str) -> nom::IResult<&str, Token> {
    alt((
        tag("==").map(|_| SpecialSymbol::Equals),
        tag(">=").map(|_| SpecialSymbol::GreaterOrEq),
        tag("<=").map(|_| SpecialSymbol::LessOrEq),
        tag("&&").map(|_| SpecialSymbol::LogicalAnd),
        tag("||").map(|_| SpecialSymbol::LogicalOr),
        tag("!=").map(|_| SpecialSymbol::NotEquals),
        single_special_symbol,
    ))
    .map(|symbol| Token {
        kind: TokenKind::SpecialSymbol(symbol),
        value: TokenValue::None,
    })
    .parse(input)
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
    let mut double = nom::number::complete::double.map(|val| Token {
        kind: TokenKind::Literal,
        value: TokenValue::Number(NumberValue::Double(val)),
    });

    let as_int = integer(input);
    let as_double = double.parse(input);

    let Ok((after_int, int_val)) = as_int else {
        // if int parsing failed -- parse as double and don't care
        return as_double;
    };

    let Ok((after_double, double_val)) = as_double else {
        // double parsing failed, but integer succeed... well,
        // return integer
        return Ok((after_int, int_val));
    };

    // both succeeded -- take one that parsed further
    Ok(if after_double.len() < after_int.len() {
        // double advanced further
        (after_double, double_val)
    } else {
        (after_int, int_val)
    })
}

fn string_payload(input: &str) -> nom::IResult<&str, &str> {
    let (rest, string_len) = fold_many0(
        alt((
            tag(r#"\""#),                                               // try eagerly consume \"
            tag(r#"\"#), // if failed, try to consume \
            take_while1(|c: char| c != '\n' && c != '\"' && c != '\\'), // consume any other characters except newline
        )),
        || 0,
        |acc: usize, item: &str| acc + item.len(),
    )
    .parse(input)?;
    Ok((rest, &input[..string_len]))
}

fn string_literal(input: &str) -> nom::IResult<&str, Token> {
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
