use crate::error::{ErrorKind, PResult};

use super::{
    token::{Assoc, Keyword, Token},
    ExprParser, Expression, Operator, Parser,
};

impl<'src> ExprParser<'src> for Parser<'src> {
    fn parse_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        self.parse_expr_with_precedence(1)
    }

    fn parse_expr_with_precedence(
        &mut self,
        min_prec: u8,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        let mut lhs = match self.parse_expr_lhs()? {
            None => return Ok(None),
            Some(expr) => expr,
        };

        while let Some(Token::Op(op)) = self.lexer.peek() {
            let op = op.clone();
            let op_prec = op.get_precedence();

            if op_prec < min_prec {
                break;
            }
            self.bump();

            let new_min_prec = if op.get_assoc() == Assoc::Left {
                op_prec + 1
            } else {
                op_prec
            };

            let rhs = match self.parse_expr_with_precedence(new_min_prec)? {
                None => {
                    return Err(ErrorKind::ParseError(
                        "Expected expression, found EOF".into(),
                    ))
                }
                Some(expr) => expr,
            };

            lhs = Box::new(Expression::Binary { lhs, op, rhs });
        }

        Ok(Some(lhs))
    }

    fn parse_expr_lhs(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        match self.lexer.next() {
            None => Ok(None),
            Some(token) => match token {
                Token::Number(v) => Ok(Some(Box::new(Expression::Number(v)))),
                Token::Id(id) => {
                    if let Some(Token::LParen) = self.lexer.peek() {
                        self.eat();
                        return Ok(Some(self.parse_call_expr(id)?));
                    }

                    Ok(Some(Box::new(Expression::VarRef(id))))
                }
                Token::LParen => self.parse_grouping_expr(),
                Token::Op(Operator::Minus) => self.parse_unary_expr(),
                Token::Kw(Keyword::True) => Ok(Some(Box::new(Expression::Boolean(true)))),
                Token::Kw(Keyword::False) => Ok(Some(Box::new(Expression::Boolean(false)))),
                other => Err(ErrorKind::ParseError(format!(
                    "Expected expression, found {other:?}"
                ))),
            },
        }
    }

    fn parse_grouping_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        if let Some(expression) = self.parse_expr()? {
            self.expect(Token::RParen)?;
            return Ok(Some(Box::new(Expression::Grouping(expression))));
        }
        Err(ErrorKind::ParseError("Expected `)`, found EOF".to_string()))
    }

    fn parse_unary_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        if let Some(expression) = self.parse_expr_lhs()? {
            return Ok(Some(Box::new(Expression::Unary(expression))));
        }
        Err(ErrorKind::ParseError(
            "Expected expression, found EOF".to_string(),
        ))
    }

    fn parse_call_expr(&mut self, id: &'src str) -> PResult<Box<Expression<'src>>> {
        let mut args = vec![];

        match self.lexer.peek() {
            Some(Token::RParen) => (),
            _ => {
                let expr = self.expect_expr()?;
                args.push(*expr);

                loop {
                    match self.lexer.peek() {
                        None => {
                            return Err(ErrorKind::ParseError(
                                "Expected `,` or `)`, found EOF".into(),
                            ))
                        }
                        Some(Token::RParen) => break,
                        Some(Token::Comma) => {
                            self.eat();
                            let expr = self.expect_expr()?;
                            args.push(*expr);
                        }
                        other => {
                            return Err(ErrorKind::ParseError(format!(
                                "Expected `,` or `)`, found {other:?}"
                            )))
                        }
                    }
                }
            }
        }
        self.eat();
        Ok(Box::new(Expression::FnCall { id, params: args }))
    }
}
