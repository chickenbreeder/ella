mod expr;
mod expr_parser;
mod lexer;
mod parser;
pub(crate) mod stmt;
mod token;

pub(crate) use expr::Expression;
pub(crate) use parser::Parser;
pub(crate) use token::Operator;

use crate::error::PResult;

pub(crate) trait ExprParser<'src> {
    fn parse_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_expr_with_precedence(
        &mut self,
        min_prec: u8,
    ) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_expr_lhs(&mut self) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_grouping_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_unary_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>>;
    fn parse_call_expr(&mut self, id: &'src str) -> PResult<Box<Expression<'src>>>;
}
