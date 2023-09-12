use crate::{
    error::{ErrorKind, PResult},
    expr::Expression,
    lexer::Lexer,
    stmt::{FnDecl, Statement},
    token::{Assoc, Keyword, Operator, Token},
};
use std::iter::Peekable;

pub(crate) struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn parse_stmt(&mut self) -> PResult<Option<Box<Statement<'src>>>> {
        match self.lexer.next() {
            None => Ok(None),
            Some(Token::Kw(kw)) => match kw {
                Keyword::Let => {
                    let id = self.parse_id()?;
                    self.expect(Token::Eq)?;
                    let expr = self.expect_expr()?;
                    self.expect(Token::Semicolon)?;

                    Ok(Some(Box::new(Statement::VarDecl { id, value: expr })))
                }
                Keyword::Return => {
                    let expr = self.expect_expr()?;
                    self.expect(Token::Semicolon)?;

                    Ok(Some(Box::new(Statement::Return(expr))))
                }
                Keyword::Fn => {
                    let id = self.parse_id()?;
                    let mut statements = vec![];

                    self.expect(Token::LParen)?;
                    self.expect(Token::RParen)?;
                    self.expect(Token::LCurly)?;

                    loop {
                        if let Some(Token::RCurly) = self.lexer.peek() {
                            self.eat();
                            break;
                        }

                        let stmt = match self.parse_stmt()? {
                            Some(stmt) => stmt,
                            None => {
                                return Err(ErrorKind::ParseError(
                                    "Expected statement or `}`, found EOF".into(),
                                ))
                            }
                        };
                        statements.push(*stmt);
                    }

                    Ok(Some(Box::new(Statement::FnDecl(FnDecl {
                        id,
                        arity: 0,
                        body: statements,
                    }))))
                }
                _ => todo!(),
            },
            Some(Token::Id(id)) => {
                self.expect(Token::Eq)?;
                let expr = self.expect_expr()?;
                self.expect(Token::Semicolon)?;
                Ok(Some(Box::new(Statement::Assignment { id, value: expr })))
            }
            other => Err(ErrorKind::ParseError(format!(
                "Expected statement, found {other:?}"
            ))),
        }
    }

    fn parse_id(&mut self) -> PResult<&'src str> {
        match self.lexer.next() {
            None => Err(ErrorKind::ParseError("Expected id, found EOF".into())),
            Some(Token::Id(id)) => Ok(id),
            other => Err(ErrorKind::ParseError(format!(
                "Expected id, found {other:?}"
            ))),
        }
    }

    pub fn parse_expr(&mut self) -> PResult<Option<Box<Expression<'src>>>> {
        self.parse_expr_with_precedence(1)
    }

    #[inline(always)]
    fn bump(&mut self) {
        let _ = self.lexer.next();
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
                Token::Id(id) => Ok(Some(Box::new(Expression::VarRef(id)))),
                Token::LParen => self.parse_grouping_expr(),
                Token::Op(Operator::Minus) => self.parse_unary_expr(),
                other => Err(ErrorKind::ParseError(format!(
                    "Expected expression, found {other:?}"
                ))),
            },
        }
    }

    fn expect(&mut self, expected: Token) -> PResult<()> {
        match self.lexer.next() {
            None => Err(ErrorKind::ParseError(format!(
                "Expected {expected:?}, found EOF"
            ))),
            Some(token) => {
                if token == expected {
                    return Ok(());
                }
                Err(ErrorKind::ParseError(format!(
                    "Expected {expected:?}, found {token:?}"
                )))
            }
        }
    }

    fn eat(&mut self) {
        let _ = self.lexer.next();
    }

    fn expect_expr(&mut self) -> PResult<Box<Expression<'src>>> {
        match self.parse_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ErrorKind::ParseError(
                "Expected expression, found EOF".into(),
            )),
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
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::{expr::Expression, stmt::Statement, token::Operator};

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

    #[test]
    fn parse_let_stmt() {
        let mut parser = Parser::new("let a = 42;");
        let expr = parser.parse_stmt().unwrap().unwrap();
        let expected = Box::new(Statement::VarDecl {
            id: "a",
            value: Box::new(Expression::Number(42)),
        });

        assert_eq!(expr, expected);
    }
}
