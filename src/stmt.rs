use crate::expr::Expression;

#[derive(Debug)]
pub(crate) enum Statement<'src> {
    VarDecl {
        id: &'src str,
        value: Box<Expression<'src>>,
    },
}
