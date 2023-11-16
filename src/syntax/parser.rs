use crate::{
    error::{ErrorKind, PResult},
    syntax::{
        lexer::Lexer,
        stmt::{FnDecl, FnType, Statement},
        token::{Keyword, Token},
        ExprParser, Expression,
    },
};
use std::{collections::HashMap, iter::Peekable};

use super::{scope::ScopeEnv, FunctionIndex, LocalIndex};

pub(crate) struct Parser<'src> {
    pub(super) lexer: Peekable<Lexer<'src>>,
    fn_index: FunctionIndex,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src).peekable(),
            fn_index: 0,
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
                let locals: HashMap<&'src str, LocalIndex> = params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| (*p, i as LocalIndex))
                    .collect();

                let env = ScopeEnv::with_locals(locals);

                match *self.parse_scope(Some(env))? {
                    Statement::Block(_, statements) => {
                        let index = self.fn_index;
                        self.fn_index += 1;

                        Ok(Some(Box::new(Statement::FnDecl(FnDecl {
                            id,
                            arity: params.len() as u8,
                            ty: FnType::NativeFn {
                                index,
                                params,
                                body: statements,
                            },
                        }))))
                    }
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

    fn parse_stmt(&mut self, env: &mut ScopeEnv<'src>) -> PResult<Option<Box<Statement<'src>>>> {
        match self.lexer.peek() {
            None => Ok(None),
            Some(Token::Kw(kw)) => {
                let kw = *kw;
                self.bump();

                match kw {
                    Keyword::Let => {
                        let id = self.parse_id()?;
                        self.expect(Token::Eq)?;
                        let expr = self.expect_expr(env)?;
                        self.expect(Token::Semicolon)?;

                        let index = env.add(id)?;

                        Ok(Some(Box::new(Statement::LetDecl {
                            id,
                            index,
                            value: expr,
                        })))
                    }
                    Keyword::Return => {
                        let expr = self.expect_expr(env)?;
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
                    let expr = self.parse_call_expr(env, id)?;
                    self.expect(Token::Semicolon)?;
                    return Ok(Some(Box::new(Statement::FnCall(expr))));
                }

                self.expect(Token::Eq)?;
                let expr = self.expect_expr(env)?;
                self.expect(Token::Semicolon)?;

                let index = env.get(id)?;

                Ok(Some(Box::new(Statement::Assignment {
                    id,
                    index,
                    value: expr,
                })))
            }
            Some(Token::LCurly) => {
                let stmt = self.parse_scope(None)?;
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

    fn parse_scope(
        &mut self,
        parent_scope: Option<ScopeEnv<'src>>,
    ) -> PResult<Box<Statement<'src>>> {
        let mut statements = vec![];
        let mut env = if let Some(parent_scope) = parent_scope {
            parent_scope
        } else {
            ScopeEnv::new()
        };

        self.expect(Token::LCurly)?;

        loop {
            if let Some(Token::RCurly) = self.lexer.peek() {
                self.eat();
                break;
            }

            let stmt = match self.parse_stmt(&mut env)? {
                Some(stmt) => stmt,
                None => {
                    return Err(ErrorKind::ParseError(
                        "Expected statement or `}`, found EOF".into(),
                    ))
                }
            };
            statements.push(*stmt);
        }

        Ok(Box::new(Statement::Block(env, statements)))
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

    pub(super) fn expect_expr(
        &mut self,
        env: &mut ScopeEnv<'src>,
    ) -> PResult<Box<Expression<'src>>> {
        match self.parse_expr(env)? {
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
    use crate::syntax::{expr::Expression, scope::ScopeEnv, stmt::Statement};

    #[test]
    fn parse_let_stmt() {
        let mut parser = Parser::new("let a = 42;");
        let mut env = ScopeEnv::new();
        let expr = parser.parse_stmt(&mut env).unwrap().unwrap();
        let expected = Box::new(Statement::LetDecl {
            id: "a",
            index: 0,
            value: Box::new(Expression::Number(42)),
        });

        assert_eq!(expr, expected);
    }
}
