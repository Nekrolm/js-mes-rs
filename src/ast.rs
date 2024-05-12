use std::rc::Rc;

use crate::lexer::NumberValue;


pub enum Statement<'a> {
    VarDecl(VariableDeclaration<'a>)
}


pub struct Node {

}



#[derive(Debug)]
pub struct VariableDeclaration<'a> {
    pub var_name: &'a str,
    pub init_expression: Expression<'a>,
}

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Plus,
    Minus,
    Multiple,
    Divide,
}

#[derive(Debug)]
pub struct UnaryExpression<'a> {
    pub unary_operation: UnaryOperation,
    pub expression: Expression<'a>
}


#[derive(Debug)]
pub struct BinaryExpression<'a> {
    pub left: Expression<'a>,
    pub operation: BinaryOperation,
    pub right: Expression<'a>,
}

