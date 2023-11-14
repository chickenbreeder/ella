use crate::{
    error::{ErrorKind, PResult},
    syntax::{
        lexer::Lexer,
        stmt::{FnDecl, FnType, Statement},
        token::{Keyword, Token},
        ExprParser, Expression,
    },
};
use std::iter::Peekable;

pub(crate) struct Parser<'src> {
    pub(super) lexer: Peekable<Lexer<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
        }
    }

    pub fn parse_top_level_stmt(&mut self) -> PResult<Option<Box<Statement<'src>>>> {
        match self.lexer.peek() {
            None => Ok(None),
            Some(Token::Kw(Keyword::Fn)) => {
                self.bump();

                let id = self.parse_id()?;
                self.expect(Token::LParen)?;
                let params = self.parse_fn_params()?;

                match *self.parse_block()? {
                    Statement::Block(statements) => Ok(Some(Box::new(Statement::FnDecl(FnDecl {
                        id,
                        arity: params.len() as u8,
                        ty: FnType::NativeFn {
                            params,
                            body: statements,
                        },
                    })))),
                    other => Err(ErrorKind::ParseError(format!(
                        "Expected a block statement, found {other:?}"
                    ))),
                }
            }
            other => Err(ErrorKind::ParseError(format!(
                "Expected top-level statement, found {other:?}"
            ))),
        }
    }

    fn parse_stmt(&mut self, local_index: &mut u32) -> PResult<Option<Box<Statement<'src>>>> {
        match self.lexer.peek() {
            None => Ok(None),
            Some(Token::Kw(kw)) => {
                let kw = *kw;
                self.bump();

                match kw {
                    Keyword::Let => {
                        let id = self.parse_id()?;
                        self.expect(Token::Eq)?;
                        let expr = self.expect_expr()?;
                        self.expect(Token::Semicolon)?;

                        let index = *local_index;
                        *local_index += 1;

                        Ok(Some(Box::new(Statement::LetDecl {
                            id,
                            index,
                            value: expr,
                        })))
                    }
                    Keyword::Return => {
                        let expr = self.expect_expr()?;
                        self.expect(Token::Semicolon)?;

                        Ok(Some(Box::new(Statement::Return(expr))))
                    }
                    other => Err(ErrorKind::ParseError(format!(
                        "Expected statement, found {other:?}"
                    ))),
                }
            }
            Some(Token::Id(id)) => {
                let id = *id;
                self.bump();

                if let Some(Token::LParen) = self.lexer.peek() {
                    self.eat();
                    let expr = self.parse_call_expr(id)?;
                    self.expect(Token::Semicolon)?;
                    return Ok(Some(Box::new(Statement::FnCall(expr))));
                }

                self.expect(Token::Eq)?;
                let expr = self.expect_expr()?;
                self.expect(Token::Semicolon)?;
                Ok(Some(Box::new(Statement::Assignment { id, value: expr })))
            }
            Some(Token::LCurly) => {
                let stmt = self.parse_block()?;
                Ok(Some(stmt))
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

    #[inline(always)]
    pub(super) fn bump(&mut self) {
        let _ = self.lexer.next();
    }

    // TODO: Refactor this and parse_call_expr to get rid of duplicated code
    fn parse_fn_params(&mut self) -> PResult<Vec<&'src str>> {
        let mut params = vec![];

        match self.lexer.peek() {
            Some(Token::RParen) => (),
            _ => {
                let param = self.parse_id()?;
                params.push(param);

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
                            let param = self.parse_id()?;
                            params.push(param);
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
        Ok(params)
    }

    fn parse_block(&mut self) -> PResult<Box<Statement<'src>>> {
        let mut statements = vec![];
        let mut local_index: u32 = 0;

        self.expect(Token::LCurly)?;

        loop {
            if let Some(Token::RCurly) = self.lexer.peek() {
                self.eat();
                break;
            }

            let stmt = match self.parse_stmt(&mut local_index)? {
                Some(stmt) => stmt,
                None => {
                    return Err(ErrorKind::ParseError(
                        "Expected statement or `}`, found EOF".into(),
                    ))
                }
            };
            statements.push(*stmt);
        }

        Ok(Box::new(Statement::Block(statements)))
    }

    pub(super) fn parse_rep<T, F>(&mut self, mut producer: F) -> PResult<Vec<T>>
    where
        F: FnMut() -> PResult<T>,
    {
        let mut result = vec![];

        match self.lexer.peek() {
            Some(Token::RParen) => (),
            _ => {
                let param = producer()?; //self.parse_id()?;
                result.push(param);

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
                            let param = producer()?;
                            result.push(param);
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
        Ok(result)
    }

    pub(super) fn expect(&mut self, expected: Token) -> PResult<()> {
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

    pub(super) fn eat(&mut self) {
        let _ = self.lexer.next();
    }

    pub(super) fn expect_expr(&mut self) -> PResult<Box<Expression<'src>>> {
        match self.parse_expr()? {
            Some(expr) => Ok(expr),
            None => Err(ErrorKind::ParseError(
                "Expected expression, found EOF".into(),
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::syntax::{expr::Expression, stmt::Statement};

    #[test]
    fn parse_let_stmt() {
        let mut parser = Parser::new("let a = 42;");
        let mut index = 0;
        let expr = parser.parse_stmt(&mut index).unwrap().unwrap();
        let expected = Box::new(Statement::LetDecl {
            id: "a",
            index: 0,
            value: Box::new(Expression::Number(42)),
        });

        assert_eq!(expr, expected);
    }
}
