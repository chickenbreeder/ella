use crate::error::{ErrorKind, PResult};

use super::{
    scope::ScopeEnv,
    token::{Assoc, Keyword, Precedence, Token},
    ExprParser, Expression, Operator, Parser,
};

impl<'src> ExprParser<'src> for Parser<'src> {
    fn parse_expr(&mut self, env: &mut ScopeEnv<'src>) -> PResult<Option<Box<Expression<'src>>>> {
        self.parse_expr_with_precedence(env, 1)
    }

    fn parse_expr_with_precedence(
        &mut self,
        env: &mut ScopeEnv<'src>,
        min_prec: Precedence,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        let mut lhs = match self.parse_expr_lhs(env)? {
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

            let rhs = match self.parse_expr_with_precedence(env, new_min_prec)? {
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

    fn parse_expr_lhs(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        match self.lexer.next() {
            None => Ok(None),
            Some(token) => match token {
                Token::Number(v) => Ok(Some(Box::new(Expression::Number(v)))),
                Token::Id(id) => match self.lexer.peek() {
                    Some(Token::LParen) => {
                        self.eat();
                        Ok(Some(self.parse_call_expr(env, id)?))
                    }
                    Some(Token::LBracket) => {
                        self.eat();
                        let idx = match self.parse_expr_lhs(env)? {
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
                    _ => {
                        let index = env.get(id)?;
                        Ok(Some(Box::new(Expression::Ref { index, id })))
                    }
                },
                Token::LParen => self.parse_grouping_expr(env),
                Token::LBracket => {
                    let expr = self.parse_list_expr(env)?;
                    Ok(Some(expr))
                }
                Token::Op(Operator::Minus) => self.parse_unary_expr(env),
                Token::Kw(Keyword::True) => Ok(Some(Box::new(Expression::Boolean(true)))),
                Token::Kw(Keyword::False) => Ok(Some(Box::new(Expression::Boolean(false)))),
                other => Err(ErrorKind::ParseError(format!(
                    "Expected expression, found {other:?}"
                ))),
            },
        }
    }

    fn parse_grouping_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        if let Some(expression) = self.parse_expr(env)? {
            self.expect(Token::RParen)?;
            return Ok(Some(Box::new(Expression::Grouping(expression))));
        }
        Err(ErrorKind::ParseError("Expected `)`, found EOF".to_string()))
    }

    fn parse_unary_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Option<Box<Expression<'src>>>> {
        if let Some(expression) = self.parse_expr_lhs(env)? {
            return Ok(Some(Box::new(Expression::Unary(expression))));
        }
        Err(ErrorKind::ParseError(
            "Expected expression, found EOF".to_string(),
        ))
    }

    fn parse_call_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
        id: &'src str,
    ) -> PResult<Box<Expression<'src>>> {
        let index = match self.functions.get(id) {
            Some(index) => *index,
            None => {
                return Err(ErrorKind::ParseError(format!(
                    "Undefined function `{id}` (!)"
                )))
            }
        };

        let mut args = vec![];

        match self.lexer.peek() {
            Some(Token::RParen) => (),
            _ => {
                let expr = self.expect_expr(env)?;
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
                            let expr = self.expect_expr(env)?;
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
        Ok(Box::new(Expression::Call {
            id,
            index,
            params: args,
        }))
    }

    fn parse_list_expr(&mut self, env: &mut ScopeEnv<'src>) -> PResult<Box<Expression<'src>>> {
        let mut args = vec![];

        match self.lexer.peek() {
            Some(Token::RBracket) => (),
            _ => {
                let expr = self.expect_expr(env)?;
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
                            let expr = self.expect_expr(env)?;
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
    use crate::syntax::{expr::Expression, scope::ScopeEnv, token::Operator, ExprParser};

    #[test]
    fn parse_binary_expr() {
        use Expression::*;
        use Operator::*;

        let mut env = ScopeEnv::new();
        let mut parser = Parser::new("-5 + 4 * 7");
        let expr = parser.parse_expr(&mut env).unwrap().unwrap();
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

        let mut env = ScopeEnv::new();
        let mut parser = Parser::new("(-5 + 4) * 7");
        let expr = parser.parse_expr(&mut env).unwrap().unwrap();
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
