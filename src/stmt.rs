use crate::expr::Expression;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct FnDecl<'src> {
    pub id: &'src str,
    pub arity: u8,
    pub body: Vec<Statement<'src>>,
}

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
    FnDecl(FnDecl<'src>),
    FnCall(Box<Expression<'src>>),
}
