use crate::token::OperatorKind;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Expression<'src> {
    Number(i64),
    Grouping(Box<Expression<'src>>),
    Unary(Box<Expression<'src>>),
    Binary {
        lhs: Box<Expression<'src>>,
        op: OperatorKind,
        rhs: Box<Expression<'src>>,
    },
    VarRef(&'src str),
}
