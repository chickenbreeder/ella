use std::collections::HashMap;

use super::env::Environment;
use crate::{
    error::{ErrorKind, PResult},
    expr::Expression,
    parser::Parser,
    stmt::{FnDecl, FnType, Statement},
    token::Operator,
};

pub(crate) struct Interpreter<'src> {
    functions: HashMap<&'src str, FnDecl<'src>>,
}

fn native_print(arg: i64) -> i64 {
    println!("{}", arg);
    1
}

fn native_assert(arg0: i64, arg1: i64) -> i64 {
    assert_eq!(arg0, arg1);
    1
}

impl<'src> Interpreter<'src> {
    pub fn new() -> Self {
        let mut functions = HashMap::new();
        functions.insert(
            "print",
            FnDecl {
                id: "print",
                arity: 1,
                ty: FnType::NativeFn { func: native_print },
            },
        );

        Self { functions }
    }

    pub fn eval(&mut self, src: &'src str) -> PResult<()> {
        let mut parser = Parser::new(src);

        while let Some(stmt) = parser.parse_stmt()? {
            self.eval_top_level_stmt(stmt)?;
        }

        let _ = self.eval_fn_call("main", &[])?;
        Ok(())
    }

    #[allow(dead_code)]
    pub fn eval_expr_str(&mut self, expr: &'static str) -> PResult<Option<i64>> {
        let mut parser = Parser::new(expr);
        let mut global_env = Environment::new();

        match parser.parse_expr()? {
            Some(expr) => Ok(Some(self.eval_expr_in_env(&expr, &mut global_env)?)),
            None => Ok(None),
        }
    }

    fn eval_top_level_stmt(&mut self, stmt: Box<Statement<'src>>) -> PResult<()> {
        match *stmt {
            Statement::FnDecl(decl) => {
                self.functions.insert(decl.id, decl);
            }
            other => {
                return Err(ErrorKind::RuntimeError(format!(
                    "Expected top level statement, found {other:?}"
                )))
            }
        }
        Ok(())
    }

    fn eval_stmt_in_env(&self, stmt: &Statement<'src>, env: &mut Environment<'src>) -> PResult<()> {
        match stmt {
            Statement::VarDecl { id, value } => {
                let value = self.eval_expr_in_env(&value, env)?;
                println!("DECL: {id} = {value}");

                if env.contains_key(id) {
                    return Err(ErrorKind::RuntimeError(format!(
                        "Variable `{id}` has already been declared"
                    )));
                } else {
                    env.insert(id, value);
                }
            }
            Statement::Assignment { id, value } => {
                let value = self.eval_expr_in_env(&value, env)?;

                if let Some(v) = env.get_mut(id) {
                    *v = value;
                } else {
                    return Err(ErrorKind::RuntimeError(format!(
                        "Undefined reference `{id}`"
                    )));
                }
            }
            Statement::FnCall(expr) => {
                if let Expression::FnCall { id, ref params } = **expr {
                    let args: Vec<i64> = params
                        .iter()
                        .map(|p| self.eval_expr_in_env(p, env).unwrap())
                        .collect();
                    self.eval_fn_call(id, &args)?;
                } else {
                    unreachable!("This should never happen")
                }
            }
            other => {
                return Err(ErrorKind::RuntimeError(format!(
                    "Expected statement, found {other:?}"
                )))
            }
        }
        Ok(())
    }

    fn eval_expr_in_env(&self, expr: &Expression, env: &mut Environment<'src>) -> PResult<i64> {
        match expr {
            Expression::Number(v) => Ok(*v),
            Expression::Grouping(expr) => self.eval_expr_in_env(expr, env),
            Expression::Unary(expr) => Ok(-self.eval_expr_in_env(expr, env)?),
            Expression::Binary { lhs, op, rhs } => {
                let lhs = self.eval_expr_in_env(lhs, env)?;
                let rhs = self.eval_expr_in_env(rhs, env)?;

                match op {
                    Operator::Plus => Ok(lhs + rhs),
                    Operator::Minus => Ok(lhs - rhs),
                    Operator::Mul => Ok(lhs * rhs),
                }
            }
            Expression::VarRef(id) => {
                if let Some(value) = env.get(id) {
                    return Ok(*value);
                }
                Err(ErrorKind::RuntimeError(format!(
                    "Undefined reference `{id}`"
                )))
            }
            Expression::FnCall { id, params } => {
                let args: Vec<i64> = params
                    .iter()
                    .map(|p| self.eval_expr_in_env(p, env).unwrap())
                    .collect();
                self.eval_fn_call(id, &args)
            }
        }
    }

    fn eval_fn_call(&self, id: &'src str, args: &[i64]) -> PResult<i64> {
        let decl = match self.functions.get(id) {
            Some(decl) => decl,
            None => {
                return Err(ErrorKind::RuntimeError(format!(
                    "Undefined function `{id}`"
                )))
            }
        };

        if decl.arity != args.len() as u8 {
            return Err(ErrorKind::RuntimeError(format!(
                "Argument count missmatch. Expected {}, got {}",
                decl.arity,
                args.len()
            )));
        }

        let mut env = Environment::new();

        match &decl.ty {
            FnType::NativeFn { func } => {
                return Ok(func(args[0]));
            }
            FnType::NormalFn { params, body } => {
                let statements = &body[..body.len() - 1];

                for i in 0..decl.arity as usize {
                    let param = params[i];
                    let argument = args[i]; //self.eval_expr_in_env(&args[i], &mut env)?;

                    env.insert(param, argument);
                }

                for stmt in statements {
                    self.eval_stmt_in_env(stmt, &mut env)?;
                }

                match body.last() {
            Some(Statement::Return(expr)) => Ok(self.eval_expr_in_env(expr, &mut env)?),
            _ => unreachable!("Last statement in function body was not a return statement. This should never happen")
        }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Interpreter;

    #[test]
    fn eval_1() {
        let value = Interpreter::new()
            .eval_expr_str("4 + 10 * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, -6);
    }

    #[test]
    fn eval_2() {
        let value = Interpreter::new()
            .eval_expr_str("(4 + 10) * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, -14);
    }

    #[test]
    fn eval_3() {
        let value = Interpreter::new()
            .eval_expr_str("-48 + 9")
            .unwrap()
            .unwrap();
        assert_eq!(value, -39);
    }

    #[test]
    fn eval_4() {
        let value = Interpreter::new()
            .eval_expr_str("-8 + 5 * (13 - 1) * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, -68);
    }

    #[test]
    fn eval_5() {
        let value = Interpreter::new()
            .eval_expr_str("(-8 + 5) * (13 - 1) * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, 36);
    }
}
