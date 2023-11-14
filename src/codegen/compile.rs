use wasm_encoder::{CodeSection, Function, Instruction, Module, ValType};

use crate::{
    error::{ErrorKind, PResult},
    syntax::{
        stmt::{FnType, Statement},
        Expression, Parser,
    },
};

pub(crate) struct Compiler<'src> {
    parser: Parser<'src>,
}

impl<'src> Compiler<'src> {
    pub fn from_src(src: &'src str) -> Self {
        Self {
            parser: Parser::new(src),
        }
    }

    pub fn compile(&mut self) -> PResult<Vec<u8>> {
        let mut module = Module::new();
        let mut code = CodeSection::new();

        match self.parser.parse_top_level_stmt()? {
            None => (),
            Some(stmt) => match *stmt {
                Statement::FnDecl(decl) => match &decl.ty {
                    FnType::ForeignFn { func } => {
                        return Err(ErrorKind::ParseError(
                            "Cannot compile a foreign function".into(),
                        ))
                    }
                    FnType::NativeFn { params: _, body } => {
                        let f = Self::compile_fn(&body);
                        code.function(&f);
                    }
                },
                other => {
                    return Err(ErrorKind::ParseError(format!(
                        "Expected function, found {other:?}"
                    )))
                }
            },
        }

        module.section(&code);
        Ok(module.finish())
    }

    fn compile_fn(statements: &[Statement<'src>]) -> Function {
        let mut locals = vec![];
        let mut instructions = vec![];

        for s in statements {
            match s {
                Statement::LetDecl { id, index, value } => {
                    let local = Self::compile_let_decl(id, *index, value, &mut instructions);
                    locals.push(local);
                }
                _ => (),
            }
        }

        let mut f = Function::new(locals);

        instructions.iter().for_each(|i| {
            f.instruction(i);
        });

        f
    }

    fn compile_let_decl(
        id: &'src str,
        index: u32,
        expr: &Expression<'src>,
        instructions: &mut Vec<Instruction<'src>>,
    ) -> (u32, ValType) {
        let local = (index, ValType::I64);
        log::debug!("{id} => {local:?}");

        match expr {
            Expression::Number(v) => {
                instructions.push(Instruction::I64Const(*v));
                instructions.push(Instruction::LocalSet(index));
            }
            _ => unimplemented!(),
        };

        local
    }
}

#[cfg(test)]
mod test {
    use super::Compiler;

    #[test]
    fn simple() {
        let mut compiler = Compiler::from_src("fn foo() { let a = 42; let b = 18; }");
        let bytes = compiler.compile().unwrap();

        println!("{bytes:X?}");
    }
}
