use crate::{runtime::value::Value, syntax::expr::Expression};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum FnType<'src> {
    ForeignFn {
        func: fn(Value) -> Value,
    },
    NativeFn {
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
    LetDecl {
        id: &'src str,
        index: u32,
        value: Box<Expression<'src>>,
    },
    Assignment {
        id: &'src str,
        value: Box<Expression<'src>>,
    },
    Return(Box<Expression<'src>>),
    Block(Vec<Statement<'src>>),
    FnDecl(FnDecl<'src>),
    FnCall(Box<Expression<'src>>),
    If {
        condition: Box<Expression<'src>>,
        else_condition: Option<Box<Expression<'src>>>,
    },
}
