use crate::{runtime::value::Value, syntax::expr::Expression};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum FnType<'src> {
    NativeFn {
        func: fn(Value) -> Value,
    },
    NormalFn {
        params: Vec<&'src str>,
        body: Vec<Statement<'src>>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct FnDecl<'src> {
    pub id: &'src str,
    pub arity: u8,
    pub ty: FnType<'src>,
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
    If {
        condition: Box<Expression<'src>>,
        else_condition: Option<Box<Expression<'src>>>,
    },
}
