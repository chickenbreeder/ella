mod expr;
mod expr_parser;
mod lexer;
mod parser;
pub(crate) mod scope;
pub(crate) mod stmt;
mod token;

pub(crate) use expr::Expression;
pub(crate) use parser::Parser;
pub(crate) use token::Operator;

use crate::error::PResult;

use self::{scope::ScopeEnv, token::Precedence};

pub(crate) type FunctionIndex = u32;
pub(crate) type LocalIndex = u32;

pub(crate) trait ExprParser<'src> {
    fn parse_expr(&mut self, env: &mut ScopeEnv<'src>) -> PResult<Option<Box<Expression<'src>>>>;

    fn parse_expr_with_precedence(
        &mut self,
        env: &mut ScopeEnv<'src>,
        min_prec: Precedence,
    ) -> PResult<Option<Box<Expression<'src>>>>;

    fn parse_expr_lhs(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_grouping_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_unary_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_call_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
        id: &'src str,
    ) -> PResult<Box<Expression<'src>>>;
    fn parse_list_expr(&mut self, env: &mut ScopeEnv<'src>) -> PResult<Box<Expression<'src>>>;
}
