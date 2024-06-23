use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};

use anyhow::Context;

use crate::{
    ast::{program, If, Statement, VariableAssignment, VariableDeclaration, VariableModifier},
    expression::{
        BinaryExpression, BinaryOperation, Expression, Identifier, LiteralExpression,
        UnaryExpression, UnaryOperation,
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

#[derive(Debug, Clone)]
pub enum ValueStorage<'a> {
    Const(Value<'a>),
    Var(Value<'a>),
}

impl<'a> ValueStorage<'a> {
    fn into_value(self) -> Value<'a> {
        match self {
            Self::Const(v) | Self::Var(v) => v,
        }
    }

    fn new(modifier: VariableModifier, value: Value<'a>) -> Self {
        match modifier {
            VariableModifier::Const => Self::Const(value),
            VariableModifier::Var => Self::Var(value),
        }
    }
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

#[derive(Default, Debug)]
pub struct Scope<'a> {
    variables: HashMap<Identifier<'a>, ValueStorage<'a>>,
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

    pub fn exec(&self) -> anyhow::Result<Scope<'a>> {
        let mut state = Scope::default();

        execute_block(&self.code, &mut state)?;

        Ok(state)
    }
}

fn execute_block<'a>(code: &[Statement<'a>], state: &mut Scope<'a>) -> anyhow::Result<()> {
    code.iter().try_for_each(|st| execute_statement(st, state))
}

fn execute_statement<'a>(st: &Statement<'a>, state: &mut Scope<'a>) -> anyhow::Result<()> {
    match st {
        Statement::VarDecl(var_decl) => execute_var_decl(var_decl, state),
        Statement::Assignment(var_assign) => execute_var_assign(var_assign, state),
        Statement::If(if_st) => execute_if_statement(if_st, state),
    }
}

fn evaluate_expression<'a>(
    expr: &Expression<'a>,
    state: &mut Scope<'a>,
) -> anyhow::Result<Value<'a>> {
    match expr {
        Expression::Identifier(ident) => state
            .variables
            .get(ident)
            .cloned()
            .map(ValueStorage::into_value)
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

    fn add(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a + b)),
            (Value::Double(a), Value::Integer(b)) => Ok(Value::Double(a + *b as f64)),
            (Value::Integer(a), Value::Double(b)) => Ok(Value::Double(a as f64 + *b)),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::String(a), Value::String(b)) => Ok(Value::String((a.into_owned() + b).into())),
            (a, b) => anyhow::bail!("Cannot add {a:?} and {b:?} -- incompatible types"),
        }
    }

    fn sub(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a - b)),
            (Value::Double(a), Value::Integer(b)) => Ok(Value::Double(a - *b as f64)),
            (Value::Integer(a), Value::Double(b)) => Ok(Value::Double(a as f64 - *b)),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (a, b) => anyhow::bail!("Cannot sub {a:?} and {b:?} -- incompatible types"),
        }
    }

    fn mul(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a * b)),
            (Value::Double(a), Value::Integer(b)) => Ok(Value::Double(a * *b as f64)),
            (Value::Integer(a), Value::Double(b)) => Ok(Value::Double(a as f64 * *b)),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (a, b) => anyhow::bail!("Cannot mul {a:?} and {b:?} -- incompatible types"),
        }
    }

    fn div(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Double(a), Value::Double(b)) => Ok(Value::Double(a / b)),
            (Value::Double(a), Value::Integer(b)) => Ok(Value::Double(a / *b as f64)),
            (Value::Integer(a), Value::Double(b)) => Ok(Value::Double(a as f64 / *b)),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Double(a as f64 / *b as f64)),
            (a, b) => anyhow::bail!("Cannot div {a:?} and {b:?} -- incompatible types"),
        }
    }

    fn and(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && *b)),
            (a, b) => anyhow::bail!(
                "Cannot apply logical operation to {a:?} and {b:?} -- not a bool type"
            ),
        }
    }

    fn or(self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || *b)),
            (a, b) => anyhow::bail!(
                "Cannot apply logical operation to {a:?} and {b:?} -- not a bool type"
            ),
        }
    }
}

macro_rules! impl_compare_operation {
    ($name:ident, $op:tt) => {
        impl<'a> Value<'a> {
            fn $name (self, other: &Value<'a>) -> anyhow::Result<Value<'a>> {
                Ok(Value::Bool(match (self, other) {
                    (Value::Bool(a), Value::Bool(b)) => a $op *b,
                    (Value::Double(a), Value::Double(b)) => a $op *b,
                    (Value::Double(a), Value::Integer(b)) => a $op *b as f64,
                    (Value::Integer(a), Value::Double(b)) => (a as f64) $op *b,
                    (Value::Integer(a), Value::Integer(b)) => a $op *b,
                    (Value::String(a), Value::String(b)) => &a $op b,
                    (a, b) => anyhow::bail!("Cannot compare {a:?} and {b:?} -- incompatibe types")
                }))
            }
        }
    };
}

impl_compare_operation!(eq, ==);
impl_compare_operation!(less, <);
impl_compare_operation!(greater, >);
impl_compare_operation!(less_or_eq, <=);
impl_compare_operation!(greater_or_eq, >=);
impl_compare_operation!(not_eq, !=);

fn evaluate_unary_expression<'a>(
    expr: &UnaryExpression<'a>,
    state: &mut Scope<'a>,
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
    state: &mut Scope<'a>,
) -> anyhow::Result<Value<'a>> {
    let left = evaluate_expression(&expr.left, state)?;
    let right = evaluate_expression(&expr.right, state)?;
    match expr.operation {
        BinaryOperation::Plus => left.add(&right),
        BinaryOperation::Minus => left.sub(&right),
        BinaryOperation::Multiple => left.mul(&right),
        BinaryOperation::Divide => left.div(&right),
        BinaryOperation::Equals => left.eq(&right),
        BinaryOperation::NotEquals => left.not_eq(&right),
        BinaryOperation::Less => left.less(&right),
        BinaryOperation::Greater => left.greater(&right),
        BinaryOperation::LessOrEq => left.less_or_eq(&right),
        BinaryOperation::GreaterOrEq => left.greater_or_eq(&right),
        BinaryOperation::LogicalAnd => left.and(&right),
        BinaryOperation::LogicalOr => left.or(&right),
    }
}

fn execute_var_decl<'a>(
    var_decl: &VariableDeclaration<'a>,
    state: &mut Scope<'a>,
) -> anyhow::Result<()> {
    let var_name = var_decl.assignment.var_name;
    let value = evaluate_expression(&var_decl.assignment.expression, state)?;
    match state.variables.entry(var_name) {
        Entry::Occupied(_) => anyhow::bail!("Redeclaration: {var_name:?}"),
        Entry::Vacant(v) => {
            v.insert(ValueStorage::new(var_decl.modifier, value));
        }
    }
    Ok(())
}

fn execute_var_assign<'a>(
    var_assign: &VariableAssignment<'a>,
    state: &mut Scope<'a>,
) -> anyhow::Result<()> {
    let var_name = var_assign.var_name;
    let value = evaluate_expression(&var_assign.expression, state)?;
    match state.variables.entry(var_name) {
        Entry::Occupied(mut o) => match o.get_mut() {
            ValueStorage::Const(_) => {
                anyhow::bail!("Cannot assigne value to the const declared {var_name:?}")
            }
            ValueStorage::Var(v) => *v = value,
        },
        Entry::Vacant(_) => {
            anyhow::bail!("Cannot assing value to undeclared variable {var_name:?}")
        }
    }
    Ok(())
}

fn execute_if_statement<'a>(st: &If<'a>, state: &mut Scope<'a>) -> anyhow::Result<()> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exec() {
        let code = r#"
        var x = 12345; // comment
        const y = "string";
        const cond = (x + 55) < 10;
        var res = 0;
        if (cond || 17 - 5 > 2.0) {
           res = 80.0;
        } else {
           res = y;
        }
    "#;
        let vm = VirtualMachine::new(code).expect("valid programm");
        let scope = vm.exec().expect("vaid code");
        dbg!(scope);
    }
}
