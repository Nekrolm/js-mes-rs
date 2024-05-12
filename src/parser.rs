use std::rc::Rc;

use nom::branch::alt;
use nom::multi::fold_many0;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::Parser;

use crate::ast::BinaryExpression;
use crate::ast::BinaryOperation;
use crate::ast::Expression;
use crate::ast::LiteralExpression;
use crate::ast::UnaryOperation;
use crate::lexer::SpecialSymbol;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::lexer::TokenValue;

use super::ast;


// Expression = Term | Term ExprContinuation
// ExprContinuation = Op Term ExprContinuation | None 
// Term = GeneralFactor TermContinuation
// TermContinuation -> Op GeneralFactor TermContinuation | None
// GeneralFactor = UnaryOp Factor | Factor
// Factor = ( Expr ) | terminal
// 

fn terminal_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    alt((
        identifier_expression,
        literal_expression.map(Expression::Literal),
    ))
    .parse(tokens)
}

fn take_one_match_map<'tokens, 'a, O: 'a>(
    tokens: &'tokens [Token<'a>],
    matcher: impl FnOnce(&Token<'a>) -> Option<O>,
) -> nom::IResult<&'tokens [Token<'a>], O> {
    tokens
        .split_first()
        .and_then(|(first, rest)| matcher(first).map(|parsed| (rest, parsed)))
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(tokens, nom::error::ErrorKind::Tag)))
}

fn take_one_matches(
    pred: impl for<'a> Fn(&Token<'a>) -> bool,
) -> impl for<'a, 'tokens> Fn(&'tokens [Token<'a>]) -> nom::IResult<&'tokens [Token<'a>], Token<'a>>
{
    move |tokens| take_one_match_map(tokens, |&tok| pred(&tok).then_some(tok))
}

fn identifier_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::Comment => unreachable!("comments are filtered out!"),
        TokenKind::Identifier => match token.value {
            TokenValue::String(string) => Some(ast::Expression::Identifier(string)),
            TokenValue::None => unreachable!("literal cannot be empty"),
            TokenValue::Number(_) => unreachable!("Identifier cannot a number"),
        },
        _ => None,
    })
}

fn literal_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::LiteralExpression<'a>> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::Comment => unreachable!("comments are filtered out!"),
        TokenKind::Literal => Some(match token.value {
            TokenValue::Number(val) => LiteralExpression::Number(val),
            TokenValue::String(string) => LiteralExpression::StringLiteral(string),
            TokenValue::None => unreachable!("literal cannot be empty"),
        }),
        _ => None,
    })
}

fn unary_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    let (rest, unary_operation) = take_one_match_map(tokens, |token| match token.kind {
        TokenKind::SpecialSymbol(SpecialSymbol::Plus) => Some(UnaryOperation::Plus),
        TokenKind::SpecialSymbol(SpecialSymbol::Minus) => Some(UnaryOperation::Minus),
        _ => None,
    })?;

    let (rest, expression) = factor(rest)?;

    Ok((
        rest,
        ast::Expression::UnaryExpression(Rc::new(ast::UnaryExpression {
            unary_operation,
            expression,
        })),
    ))
}

fn paren_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    let left_paren = take_one_matches(|tok| {
        matches!(tok.kind, TokenKind::SpecialSymbol(SpecialSymbol::LeftParen))
    });
    let right_paren = take_one_matches(|tok| {
        matches!(
            tok.kind,
            TokenKind::SpecialSymbol(SpecialSymbol::RightParen)
        )
    });
    delimited(left_paren, expression, right_paren).parse(tokens)
}

fn term_operation<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], BinaryOperation> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::SpecialSymbol(SpecialSymbol::Minus) => Some(BinaryOperation::Minus),
        TokenKind::SpecialSymbol(SpecialSymbol::Plus) => Some(BinaryOperation::Plus),
        _ => None,
    })
}

fn factor_operation<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], BinaryOperation> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::SpecialSymbol(SpecialSymbol::Star) => Some(BinaryOperation::Multiple),
        TokenKind::SpecialSymbol(SpecialSymbol::Slash) => Some(BinaryOperation::Divide),
        _ => None,
    })
}

fn factor<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    alt((paren_expression, terminal_expression)).parse(tokens)
}

fn expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    let (rest, left_term) = term(tokens)?;

    let next_term = pair(term_operation, nom::combinator::cut(term));
    fold_many0(
        next_term,
        move || left_term.clone(),
        |left, (operation, right)| {
            Expression::BinaryExpression(Rc::new(BinaryExpression {
                left,
                operation,
                right,
            }))
        },
    )
    .parse(rest)
}

fn term<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], ast::Expression<'a>> {
    let mut factor_element = alt((unary_expression, factor));
    let (rest, left_factor) = factor_element(tokens)?;

    let next_factor = pair(factor_operation, nom::combinator::cut(factor_element));

    fold_many0(
        next_factor,
        move || left_factor.clone(),
        |left, (operation, right)| {
            Expression::BinaryExpression(Rc::new(BinaryExpression {
                left,
                operation,
                right,
            }))
        },
    )
    .parse(rest)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_parse_expressions() {
        let expression_str = "a + b / c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid exp;ression");
            parsed
        };
        dbg!(expr);
    }

    #[test]
    fn test_parse_expressions_with_parens() {
        let expression_str = "(a + b - c - d - g) / c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }


    #[test]
    fn test_parse_expressions_unary() {
        let expression_str = "(a + b - c - +(d) - +g) / -c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }
}
