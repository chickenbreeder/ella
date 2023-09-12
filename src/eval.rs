use std::collections::HashMap;

use crate::{
    error::{ErrorKind, PResult},
    expr::Expression,
    parser::Parser,
    token::Operator,
};

pub(crate) struct Interpreter<'src> {
    env: HashMap<&'src str, i64>,
}

impl<'src> Interpreter<'src> {
    pub fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn eval(&self, src: &str) -> PResult<Option<i64>> {
        let mut parser = Parser::new(src);

        match parser.parse_expr()? {
            None => Ok(None),
            Some(expr) => Ok(Some(self.eval_expr(&expr)?)),
        }
    }

    fn eval_expr(&self, expr: &Expression) -> PResult<i64> {
        match expr {
            Expression::Number(v) => Ok(*v),
            Expression::Grouping(expr) => self.eval_expr(expr),
            Expression::Unary(expr) => Ok(-self.eval_expr(expr)?),
            Expression::Binary { lhs, op, rhs } => match op {
                Operator::Plus => Ok(self.eval_expr(lhs)? + self.eval_expr(rhs)?),
                Operator::Minus => Ok(self.eval_expr(lhs)? - self.eval_expr(rhs)?),
                Operator::Mul => Ok(self.eval_expr(lhs)? * self.eval_expr(rhs)?),
            },
            Expression::VarRef(id) => {
                if let Some(value) = self.env.get(id) {
                    return Ok(*value);
                }
                Err(ErrorKind::RuntimeError(format!(
                    "Undefined reference `{id}`"
                )))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::Interpreter;

    #[test]
    fn eval_1() {
        let value = Interpreter::new().eval("4 + 10 * -1").unwrap().unwrap();
        assert_eq!(value, -6);
    }

    #[test]
    fn eval_2() {
        let value = Interpreter::new().eval("(4 + 10) * -1").unwrap().unwrap();
        assert_eq!(value, -14);
    }

    #[test]
    fn eval_3() {
        let value = Interpreter::new().eval("-48 + 9").unwrap().unwrap();
        assert_eq!(value, -39);
    }

    #[test]
    fn eval_4() {
        let value = Interpreter::new()
            .eval("-8 + 5 * (13 - 1) * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, -68);
    }

    #[test]
    fn eval_5() {
        let value = Interpreter::new()
            .eval("(-8 + 5) * (13 - 1) * -1")
            .unwrap()
            .unwrap();
        assert_eq!(value, 36);
    }
}
