use crate::{runtime::value::Value, syntax::expr::Expression};

use super::{scope::ScopeEnv, token::Type, FunctionIndex, LocalIndex};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum FnType<'src> {
    ForeignFn {
        func: fn(Value) -> Value,
    },
    NativeFn {
        index: FunctionIndex,
        ty: Type,
        params: Vec<TypedId<'src>>,
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
pub(crate) struct TypedId<'src> {
    pub id: &'src str,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct Scope<'src> {
    pub env: ScopeEnv<'src>,
    pub statements: Vec<Statement<'src>>,
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
    Scope(Scope<'src>),
    FnDecl(FnDecl<'src>),
    FnCall(Box<Expression<'src>>),
    If(Box<Expression<'src>>, Scope<'src>),
}
