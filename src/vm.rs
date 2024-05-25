use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};

use anyhow::Context;

use crate::{
    ast::{program, If, Statement, VariableAssignment, VariableDeclaration},
    expression::{
        BinaryExpression, Expression, Identifier, LiteralExpression, UnaryExpression,
        UnaryOperation,
    },
    lexer::{tokenize, NumberValue, Token},
};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Integer(i64),
    Double(f64),
    Bool(bool),
    String(Cow<'a, str>),
    // TODO: Object, Array, Null
}

fn literal_to_value<'a>(liter: &LiteralExpression<'a>) -> Value<'a> {
    match liter {
        LiteralExpression::StringLiteral(s) => Value::String((*s).into()),
        LiteralExpression::Number(NumberValue::Double(d)) => Value::Double(*d),
        LiteralExpression::Number(NumberValue::Integer(i)) => Value::Integer(*i as i64),
    }
}

#[derive(Debug)]
pub struct VirtualMachine<'a> {
    code: Vec<Statement<'a>>, // global_variables: HashMap<Identifier<'a>, Value<'a>>
}

#[derive(Default)]
pub struct State<'a> {
    global_varialbes: HashMap<Identifier<'a>, Value<'a>>,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(code: &'a str) -> anyhow::Result<Self> {
        let (_, tokens): (_, Vec<Token<'a>>) =
            tokenize(code).map_err(|err| anyhow::anyhow!("Failed to tokeinize: {err:?}"))?;
        let (non_parsed, code): (_, Vec<Statement<'a>>) =
            program(&tokens).map_err(|err| anyhow::anyhow!("Failed to parse ast: {err:?}"))?;
        anyhow::ensure!(
            non_parsed.is_empty(),
            "Valid JS-MES program must not contain unrecognized tokens!"
        );
        Ok(Self { code })
    }

    pub fn exec(&self) -> anyhow::Result<State<'a>> {
        let mut state = State::default();

        execute_block(&self.code, &mut state)?;

        Ok(state)
    }
}

fn execute_block<'a>(code: &[Statement<'a>], state: &mut State<'a>) -> anyhow::Result<()> {
    code.iter().try_for_each(|st| execute_statement(st, state))
}

fn execute_statement<'a>(st: &Statement<'a>, state: &mut State<'a>) -> anyhow::Result<()> {
    match st {
        Statement::VarDecl(var_decl) => execute_var_decl(var_decl, state),
        Statement::Assignment(var_assign) => execute_var_assign(var_assign, state),
        Statement::If(if_st) => execute_if_statement(if_st, state),
    }
}

fn evaluate_expression<'a>(expr: &Expression<'a>, state: &State<'a>) -> anyhow::Result<Value<'a>> {
    match expr {
        Expression::Identifier(ident) => state
            .global_varialbes
            .get(ident)
            .cloned()
            .with_context(|| format!("{ident:?} is undefined")),
        Expression::Literal(literal) => Ok(literal_to_value(literal)),
        Expression::UnaryExpression(expr) => evaluate_unary_expression(expr, state),
        Expression::BinaryExpression(expr) => evaluate_binary_expression(expr, state),
    }
}

impl<'a> Value<'a> {
    fn negate(&self) -> anyhow::Result<Value<'a>> {
        match self {
            Value::Double(d) => Ok(Value::Double(-*d)),
            Value::Integer(i) => Ok(Value::Integer(-i)),
            _ => anyhow::bail!("Cannot negate non integer value: {self:?}"),
        }
    }

    fn not(&self) -> anyhow::Result<Value<'a>> {
        let Value::Bool(b) = self else {
            anyhow::bail!("Cannot apply logical not to non bool value: {self:?}")
        };
        Ok(Value::Bool(!*b))
    }
}

fn evaluate_unary_expression<'a>(
    expr: &UnaryExpression<'a>,
    state: &State<'a>,
) -> anyhow::Result<Value<'a>> {
    let value = evaluate_expression(&expr.expression, state)?;
    match expr.unary_operation {
        UnaryOperation::Plus => Ok(value),
        UnaryOperation::Minus => value.negate(),
        UnaryOperation::LogicalNot => value.not(),
    }
}

fn evaluate_binary_expression<'a>(
    expr: &BinaryExpression<'a>,
    state: &State<'a>,
) -> anyhow::Result<Value<'a>> {
    todo!()
}

fn execute_var_decl<'a>(
    var_decl: &VariableDeclaration<'a>,
    state: &mut State<'a>,
) -> anyhow::Result<()> {
    let var_name = var_decl.assignment.var_name;
    let value = evaluate_expression(&var_decl.assignment.expression, state)?;
    match state.global_varialbes.entry(var_name) {
        Entry::Occupied(_) => anyhow::bail!("Redeclaration: {var_name:?}"),
        Entry::Vacant(v) => {
            v.insert(value);
        }
    }
    Ok(())
}

fn execute_var_assign<'a>(
    var_assign: &VariableAssignment<'a>,
    state: &mut State<'a>,
) -> anyhow::Result<()> {
    let var_name = var_assign.var_name;
    let value = evaluate_expression(&var_assign.expression, state)?;
    match state.global_varialbes.entry(var_name) {
        Entry::Occupied(mut o) => {
            o.insert(value);
        }
        Entry::Vacant(_) => {
            anyhow::bail!("Cannot assing value to undeclared variable {var_name:?}")
        }
    }
    Ok(())
}

fn execute_if_statement<'a>(st: &If<'a>, state: &mut State<'a>) -> anyhow::Result<()> {
    let condition = evaluate_expression(&st.condition, state)?;

    let Value::Bool(condition) = condition else {
        anyhow::bail!("If condition expression must produce the boolean value")
    };

    let branch = if condition {
        &st.then_block
    } else {
        &st.else_block
    };
    execute_block(branch, state)
}
