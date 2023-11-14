use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module, ValType,
};

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

        let mut functions = FunctionSection::new();
        let type_index = 0;
        functions.function(type_index);
        module.section(&functions);

        let mut exports = ExportSection::new();
        exports.export("main", ExportKind::Func, 0);
        module.section(&exports);

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
        instructions.push(Instruction::End);

        f
    }

    fn compile_let_decl(
        id: &'src str,
        local_index: u32,
        expr: &Expression<'src>,
        instructions: &mut Vec<Instruction<'src>>,
    ) -> (u32, ValType) {
        let local = (local_index, ValType::I64);
        log::debug!("{id} => {local:?}");
        Self::compile_expr(expr, instructions);
        instructions.push(Instruction::LocalSet(local_index));

        local
    }

    fn compile_expr(expr: &Expression<'src>, instructions: &mut Vec<Instruction<'src>>) {
        match expr {
            Expression::Number(v) => {
                instructions.push(Instruction::I64Const(*v));
            }
            Expression::Grouping(expr) => Self::compile_expr(expr, instructions),
            Expression::FnCall { id, params } => {}
            _ => unimplemented!(),
        };
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
