use crate::token::Operator;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Expression<'src> {
    Number(i64),
    Grouping(Box<Expression<'src>>),
    Unary(Box<Expression<'src>>),
    Binary {
        lhs: Box<Expression<'src>>,
        op: Operator,
        rhs: Box<Expression<'src>>,
    },
    VarRef(&'src str),
    FnCall {
        id: &'src str,
        params: Vec<Expression<'src>>,
    },
}
