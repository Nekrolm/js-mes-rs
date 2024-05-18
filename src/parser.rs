use crate::lexer::Token;

pub fn take_one_match_map<'tokens, 'a, O: 'a>(
    tokens: &'tokens [Token<'a>],
    matcher: impl FnOnce(&Token<'a>) -> Option<O>,
) -> nom::IResult<&'tokens [Token<'a>], O> {
    tokens
        .split_first()
        .and_then(|(first, rest)| matcher(first).map(|parsed| (rest, parsed)))
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(tokens, nom::error::ErrorKind::Tag)))
}

pub fn take_one_matches(
    pred: impl for<'a> Fn(&Token<'a>) -> bool,
) -> impl for<'a, 'tokens> Fn(&'tokens [Token<'a>]) -> nom::IResult<&'tokens [Token<'a>], Token<'a>>
{
    move |tokens| take_one_match_map(tokens, |&tok| pred(&tok).then_some(tok))
}


