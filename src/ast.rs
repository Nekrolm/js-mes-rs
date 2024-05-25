use nom::combinator::{cut, opt};
use nom::multi::{many0_count, many1_count};
use nom::sequence::{delimited, preceded, separated_pair, terminated};
use nom::Parser;
use nom::{branch::alt, multi::many0};

use crate::expression::{expression, identifier_name};
use crate::lexer::{Keyword, SpecialSymbol, TokenKind};
use crate::parser::{take_one_match_map, take_one_matches};
use crate::{expression::Expression, lexer::Token};

#[derive(Debug)]
pub enum Statement<'a> {
    VarDecl(VariableDeclaration<'a>),
    Assignment(VariableAssignment<'a>),
    If(If<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableModifier {
    Var,
    Const,
}

#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub modifier: VariableModifier,
    pub assignment: VariableAssignment<'a>,
}

#[derive(Debug)]
pub struct VariableAssignment<'a> {
    pub var_name: &'a str,
    pub expression: Expression<'a>,
}

// If expression {
//   statements;
// } else {
//   statements;
// }
#[derive(Debug)]
pub struct If<'a> {
    pub condition: Expression<'a>,
    pub then_block: Vec<Statement<'a>>,
    pub else_block: Vec<Statement<'a>>,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Semicolon;

fn statement<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Statement<'a>> {
    alt((
        variable_declaration.map(Statement::VarDecl),
        variable_assignment.map(Statement::Assignment),
        if_statement.map(Statement::If)
    ))
    .parse(tokens)
}

fn semicolon<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Semicolon> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::SpecialSymbol(SpecialSymbol::Semicolon) => Some(Semicolon),
        _ => None,
    })
}

fn variable_assignment<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], VariableAssignment<'a>> {
    let assing = take_one_matches(|token| {
        matches!(token.kind, TokenKind::SpecialSymbol(SpecialSymbol::Assign))
    });

    terminated(
        separated_pair(identifier_name, assing, expression),
        semicolon,
    )
    .map(|(var_name, expression)| VariableAssignment {
        var_name,
        expression,
    })
    .parse(tokens)
}

fn variable_modifier<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], VariableModifier> {
    take_one_match_map(tokens, |token| match token.kind {
        TokenKind::Keyword(Keyword::Var) => Some(VariableModifier::Var),
        TokenKind::Keyword(Keyword::Const) => Some(VariableModifier::Const),
        _ => None,
    })
}

fn variable_declaration<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], VariableDeclaration<'a>> {
    // modifier Ident = Expression
    variable_modifier
        .and(variable_assignment)
        .map(|(modifier, assignment)| VariableDeclaration {
            modifier,
            assignment,
        })
        .parse(tokens)
}

fn if_statement<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], If<'a>> {
    let if_kv = take_one_matches(|token| matches!(token.kind, TokenKind::Keyword(Keyword::If)));
    let else_kv = take_one_matches(|token| matches!(token.kind, TokenKind::Keyword(Keyword::Else)));

    let then_branch = preceded(if_kv, cut(expression.and(block)));
    let else_branch = opt(preceded(else_kv, cut(block))).map(Option::unwrap_or_default);

    then_branch
        .and(else_branch)
        .map(|((condition, then_block), else_block)| If {
            condition,
            then_block,
            else_block,
        })
        .parse(tokens)
}

fn block<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Vec<Statement<'a>>> {
    let left_bracket = take_one_matches(|token| {
        matches!(
            token.kind,
            TokenKind::SpecialSymbol(SpecialSymbol::LeftBracket)
        )
    });

    let right_bracket = take_one_matches(|token| {
        matches!(
            token.kind,
            TokenKind::SpecialSymbol(SpecialSymbol::RightBracket)
        )
    });

    delimited(left_bracket, program, right_bracket).parse(tokens)
}

fn program<'tokens, 'a>(
    tokens: &'tokens [Token<'a>],
) -> nom::IResult<&'tokens [Token<'a>], Vec<Statement<'a>>> {
    let start_semicolons = many0_count(semicolon);
    let end_semicolons = many0_count(semicolon);
    
    let statement_with_trailings = delimited(start_semicolons, statement, end_semicolons);
    many0(statement_with_trailings).parse(tokens)
}

#[cfg(test)]
mod tests {
    use crate::{ast::program, lexer::tokenize};

    #[test]
    fn test_parse_programm() {
        let code = "var a = 12345 + c / d; var b = 123; c = 55; d = 1; const x = 5;";

        let (non_parsed, tokens) = tokenize(code).expect("valid code");
        assert_eq!(non_parsed, "");

        let (non_parsed, program) = program(&tokens).expect("valid tokens");
        assert_eq!(non_parsed.len(), 0);

        dbg!(program);
    }


    #[test]
    fn test_parse_if() {
        let code = "if (a + b) { c = 5; } else { c = 10; };";
        let (non_parsed, tokens) = tokenize(code).expect("valid code");
        assert_eq!(non_parsed, "");

        let (non_parsed, program) = program(&tokens).expect("valid tokens");
        assert_eq!(non_parsed.len(), 0);

        dbg!(program);
    }


    #[test]
    fn test_parse_if_opt_els() {
        let code = "if (a + b) { c = 5; };";
        let (non_parsed, tokens) = tokenize(code).expect("valid code");
        assert_eq!(non_parsed, "");

        let (non_parsed, program) = program(&tokens).expect("valid tokens");
        assert_eq!(non_parsed.len(), 0);

        dbg!(program);
    }
}
