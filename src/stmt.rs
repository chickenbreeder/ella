use crate::expr::Expression;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum Statement<'src> {
    VarDecl {
        id: &'src str,
        value: Box<Expression<'src>>,
    },
    Assignment {
        id: &'src str,
        value: Box<Expression<'src>>,
    },
    Return(Box<Expression<'src>>),
}
