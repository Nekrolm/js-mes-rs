use std::rc::Rc;

use nom::branch::alt;
use nom::multi::fold_many0;
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::Parser;

use crate::lexer::NumberValue;
use crate::lexer::SpecialSymbol;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::lexer::TokenValue;

use super::ast;

use crate::parser::take_one_match_map;
use crate::parser::take_one_matches;

// Expression = Subexpression | Subexpression ExprContinuation
// ExprContinuation -> BinaryOp Subexpression ExprContinuation | None
// subexpression = UnaryOp GeneralFactor | ( Expression ) | Terminal
// Termial = Ident | Literal

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Identifier(&'a str),
    Literal(LiteralExpression<'a>),
    BinaryExpression(Rc<BinaryExpression<'a>>),
    UnaryExpression(Rc<UnaryExpression<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub enum LiteralExpression<'a> {
    StringLiteral(&'a str),
    Number(NumberValue),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperation {
    Minus,
    Plus,
    LogicalNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Plus,
    Minus,
    Multiple,
    Divide,
    Equals,
    Less,
    Greater,
    LessOrEq,
    GreaterOrEq,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub struct UnaryExpression<'a> {
    pub unary_operation: UnaryOperation,
    pub expression: Expression<'a>,
}

#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub left: Expression<'a>,
    pub operation: BinaryOperation,
    pub right: Expression<'a>,
}

impl BinaryOperation {
    pub fn precedence(self) -> u32 {
        match self {
            Self::Plus => 3,
            Self::Minus => 3,
            Self::Multiple => 4,
            Self::Divide => 4,
            Self::Equals => 2,
            Self::Less => 2,
            Self::Greater => 2,
            Self::LessOrEq => 2,
            Self::GreaterOrEq => 2,
            Self::LogicalAnd => 1,
            Self::LogicalOr => 0,
        }
    }
}

fn terminal_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    alt((
        identifier_expression,
        literal_expression.map(Expression::Literal),
    ))
    .parse(tokens)
}

pub fn identifier_name<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], &'a str> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::Comment => unreachable!("comments are filtered out!"),
        TokenKind::Identifier => match token.value {
            TokenValue::String(string) => Some(string),
            TokenValue::None => unreachable!("literal cannot be empty"),
            TokenValue::Number(_) => unreachable!("Identifier cannot a number"),
        },
        _ => None,
    })
}

fn identifier_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    identifier_name.map(Expression::Identifier).parse(tokens)
}

fn literal_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], LiteralExpression<'a>> {
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
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    let (rest, unary_operation) = take_one_match_map(tokens, |token| match token.kind {
        TokenKind::SpecialSymbol(SpecialSymbol::Plus) => Some(UnaryOperation::Plus),
        TokenKind::SpecialSymbol(SpecialSymbol::Minus) => Some(UnaryOperation::Minus),
        TokenKind::SpecialSymbol(SpecialSymbol::LogicalNot) => Some(UnaryOperation::LogicalNot),
        _ => None,
    })?;

    let (rest, expression) = subexpression(rest)?;

    Ok((
        rest,
        Expression::UnaryExpression(Rc::new(UnaryExpression {
            unary_operation,
            expression,
        })),
    ))
}

fn paren_expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
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

fn binary_operation<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], BinaryOperation> {
    take_one_match_map(tokens, |token| match token.kind {
        // Arithmetic
        TokenKind::SpecialSymbol(SpecialSymbol::Star) => Some(BinaryOperation::Multiple),
        TokenKind::SpecialSymbol(SpecialSymbol::Slash) => Some(BinaryOperation::Divide),
        TokenKind::SpecialSymbol(SpecialSymbol::Minus) => Some(BinaryOperation::Minus),
        TokenKind::SpecialSymbol(SpecialSymbol::Plus) => Some(BinaryOperation::Plus),
        // Comparisons
        TokenKind::SpecialSymbol(SpecialSymbol::Equals) => Some(BinaryOperation::Equals),
        TokenKind::SpecialSymbol(SpecialSymbol::Less) => Some(BinaryOperation::Less),
        TokenKind::SpecialSymbol(SpecialSymbol::Greater) => Some(BinaryOperation::Greater),
        TokenKind::SpecialSymbol(SpecialSymbol::LessOrEq) => Some(BinaryOperation::LessOrEq),
        TokenKind::SpecialSymbol(SpecialSymbol::GreaterOrEq) => Some(BinaryOperation::GreaterOrEq),
        // Logical
        TokenKind::SpecialSymbol(SpecialSymbol::LogicalAnd) => Some(BinaryOperation::LogicalAnd),
        TokenKind::SpecialSymbol(SpecialSymbol::LogicalOr) => Some(BinaryOperation::LogicalOr),

        _ => None,
    })
}

fn subexpression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    alt((unary_expression, paren_expression, terminal_expression)).parse(tokens)
}

fn add_operation<'a>(
    mut stack_expression: Vec<Expression<'a>>,
    operation: BinaryOperation,
) -> Vec<Expression<'a>> {
    let right = stack_expression
        .pop()
        .expect("at least two expressions in stack");
    let left = stack_expression
        .pop()
        .expect("at least two expressions in stack");
    stack_expression.push(Expression::BinaryExpression(Rc::new(BinaryExpression {
        left,
        operation,
        right,
    })));
    stack_expression
}

fn pop_if<T>(v: &mut Vec<T>, cond: impl FnOnce(&T) -> bool) -> Option<T> {
    match v.last() {
        Some(e) if cond(e) => v.pop(),
        _ => None,
    }
}

pub fn expression<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Expression<'a>> {
    let (rest, left_term) = subexpression(tokens)?;

    let operations_stack = Vec::<BinaryOperation>::new();
    let expression_stack = vec![left_term];
    let stacks = (expression_stack, operations_stack);
    let stack_init = move || stacks.clone();

    let next_term = pair(binary_operation, nom::combinator::cut(subexpression));

    fold_many0(
        next_term,
        stack_init,
        |(expressions, mut operations), (next_operation, next_expr)| {
            let current_precedence = next_operation.precedence();
            let mut expressions = std::iter::from_fn(|| {
                pop_if(&mut operations, |op| op.precedence() >= current_precedence)
            })
            .fold(expressions, add_operation);
            expressions.push(next_expr);
            operations.push(next_operation);
            (expressions, operations)
        },
    )
    .map(|(expressions, ops)| {
        ops.into_iter()
            .rev()
            .fold(expressions, add_operation)
            .pop()
            .expect("At least one element in stack expected")
    })
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
            let (_, parsed) = expression(&tokens).expect("valid expression");
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
    fn test_parse_expressions_with_unary() {
        let expression_str = "(a + b * h - c - +(d) - +g) / -c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }

    #[test]
    fn test_parse_expressions_with_comparison() {
        let expression_str = "a + 5 < b == c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }

    #[test]
    fn test_parse_expressions_with_logical_basic() {
        let expression_str = "a + 5 == b && c > 10";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }

    #[test]
    fn test_parse_expressions_with_logical_complex() {
        let expression_str = "a + 5 == b || c > 10 && --+++---!!!!-c";
        let expr = {
            let (_, tokens) = tokenize(expression_str).expect("valid expression");
            dbg!(&tokens);
            let (_, parsed) = expression(&tokens).expect("valid expression");
            parsed
        };
        dbg!(expr);
    }
}
