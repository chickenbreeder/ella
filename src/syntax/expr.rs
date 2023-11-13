use super::token::Operator;

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
    List(Vec<Expression<'src>>),
    ListAccess(&'src str, Box<Expression<'src>>),
    FnCall {
        id: &'src str,
        params: Vec<Expression<'src>>,
    },
    Boolean(bool),
}
