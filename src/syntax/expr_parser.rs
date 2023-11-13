use crate::error::{ErrorKind, PResult};

use super::{
    token::{Assoc, Keyword, Precedence, Token},
    ExprParser, Expression, Operator, Parser,
};

impl<'src> ExprParser<'src> for Parser<'src> {
    fn parse_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        self.parse_expr_with_precedence(1)
    }

    fn parse_expr_with_precedence(
        &mut self,
        min_prec: Precedence,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        let mut lhs = match self.parse_expr_lhs()? {
            None => return Ok(None),
            Some(expr) => expr,
        };

        while let Some(Token::Op(op)) = self.lexer.peek() {
            let op = *op;
            let (prec, assoc) = op.get();

            if prec < min_prec {
                break;
            }
            self.bump();

            let new_min_prec = if assoc == Assoc::Left { prec + 1 } else { prec };

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
                Token::Id(id) => match self.lexer.peek() {
                    Some(Token::LParen) => {
                        self.eat();
                        Ok(Some(self.parse_call_expr(id)?))
                    }
                    Some(Token::LBracket) => {
                        self.eat();
                        let idx = match self.parse_expr_lhs()? {
                            Some(idx_expr) => idx_expr,
                            None => {
                                return Err(ErrorKind::ParseError(
                                    "Expected expression, found EOF".into(),
                                ))
                            }
                        };

                        self.expect(Token::RBracket)?;
                        return Ok(Some(Box::new(Expression::ListAccess(id, idx))));
                    }
                    _ => Ok(Some(Box::new(Expression::VarRef(id)))),
                },
                Token::LParen => self.parse_grouping_expr(),
                Token::LBracket => {
                    let expr = self.parse_list_expr()?;
                    Ok(Some(expr))
                }
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

    fn parse_list_expr(&mut self) -> PResult<Box<Expression<'src>>> {
        let mut args = vec![];

        match self.lexer.peek() {
            Some(Token::RBracket) => (),
            _ => {
                let expr = self.expect_expr()?;
                args.push(*expr);

                loop {
                    match self.lexer.peek() {
                        None => {
                            return Err(ErrorKind::ParseError(
                                "Expected `,` or `]`, found EOF".into(),
                            ))
                        }
                        Some(Token::RBracket) => break,
                        Some(Token::Comma) => {
                            self.eat();
                            let expr = self.expect_expr()?;
                            args.push(*expr);
                        }
                        other => {
                            return Err(ErrorKind::ParseError(format!(
                                "Expected `,` or `]`, found {other:?}"
                            )))
                        }
                    }
                }
            }
        }
        self.eat();
        Ok(Box::new(Expression::List(args)))
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::syntax::{expr::Expression, token::Operator, ExprParser};

    #[test]
    fn parse_binary_expr() {
        use Expression::*;
        use Operator::*;

        let mut parser = Parser::new("-5 + 4 * 7");
        let expr = parser.parse_expr().unwrap().unwrap();
        let expected = Box::new(Binary {
            lhs: Box::new(Unary(Box::new(Number(5)))),
            op: Plus,
            rhs: Box::new(Binary {
                lhs: Box::new(Number(4)),
                op: Mul,
                rhs: Box::new(Number(7)),
            }),
        });

        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_binary_expr_2() {
        use Expression::*;
        use Operator::*;

        let mut parser = Parser::new("(-5 + 4) * 7");
        let expr = parser.parse_expr().unwrap().unwrap();
        let expected = Box::new(Binary {
            lhs: Box::new(Grouping(Box::new(Binary {
                lhs: Box::new(Unary(Box::new(Number(5)))),
                op: Plus,
                rhs: Box::new(Number(4)),
            }))),
            op: Mul,
            rhs: Box::new(Number(7)),
        });

        assert_eq!(expr, expected);
    }
}
