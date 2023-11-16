use crate::{runtime::value::Value, syntax::expr::Expression};

use super::{scope::ScopeEnv, FunctionIndex, LocalIndex};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum FnType<'src> {
    ForeignFn {
        func: fn(Value) -> Value,
    },
    NativeFn {
        index: FunctionIndex,
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
        index: LocalIndex,
        value: Box<Expression<'src>>,
    },
    Assignment {
        id: &'src str,
        index: LocalIndex,
        value: Box<Expression<'src>>,
    },
    Return(Box<Expression<'src>>),
    Block(ScopeEnv<'src>, Vec<Statement<'src>>),
    FnDecl(FnDecl<'src>),
    FnCall(Box<Expression<'src>>),
}
