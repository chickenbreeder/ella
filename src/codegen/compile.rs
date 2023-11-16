use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::{
    error::{ErrorKind, PResult},
    syntax::{
        stmt::{FnType, Statement},
        Expression, LocalIndex, Operator, Parser,
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
        let mut types = TypeSection::new();

        let mut functions = FunctionSection::new();
        let type_index = 0;
        functions.function(type_index);

        let mut exports = ExportSection::new();

        match self.parser.parse_top_level_stmt()? {
            None => (),
            Some(stmt) => match *stmt {
                Statement::FnDecl(decl) => match &decl.ty {
                    FnType::ForeignFn { func } => {
                        return Err(ErrorKind::ParseError(
                            "Cannot compile a foreign function".into(),
                        ))
                    }
                    FnType::NativeFn { params, body } => {
                        exports.export(decl.id, ExportKind::Func, 0);

                        let mut instructions = vec![];
                        Self::compile_fn_signature(&params, &mut types);
                        let locals = Self::compile_fn(&body, &mut instructions);
                        let mut f = Function::new(locals);

                        instructions.iter().for_each(|ins| {
                            f.instruction(ins);
                        });

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
        module.section(&types);
        module.section(&functions);
        module.section(&exports);
        module.section(&code);

        Ok(module.finish())
    }

    fn compile_fn(
        statements: &[Statement<'src>],
        instructions: &mut Vec<Instruction<'src>>,
    ) -> Vec<(LocalIndex, ValType)> {
        let mut locals = vec![];

        statements
            .iter()
            .for_each(|s| Self::compile_stmt(s, &mut locals, instructions));

        instructions.push(Instruction::End);

        locals
    }

    fn compile_fn_signature(params: &[&str], types: &mut TypeSection) {
        let params: Vec<ValType> = params.iter().map(|_| ValType::I32).collect();

        types.function(params, vec![ValType::I32]);
    }

    fn compile_stmt(
        stmt: &Statement<'src>,
        locals: &mut Vec<(LocalIndex, ValType)>,
        instructions: &mut Vec<Instruction<'src>>,
    ) {
        match stmt {
            Statement::LetDecl { id, index, value } => {
                let local = Self::compile_let_decl(id, *index, &value, instructions);
                locals.push(local);
            }
            Statement::Assignment {
                id: _,
                index,
                value,
            } => {
                Self::compile_expr(&value, instructions);
                instructions.push(Instruction::LocalSet(*index));
            }
            Statement::Return(expr) => {
                Self::compile_expr(expr, instructions);
                // ? instructions.push(Instruction::Return);
            }
            other => unimplemented!("{other:?}"),
        }
    }

    fn compile_let_decl(
        id: &'src str,
        local_index: LocalIndex,
        expr: &Expression<'src>,
        instructions: &mut Vec<Instruction<'src>>,
    ) -> (LocalIndex, ValType) {
        let local = (local_index, ValType::I32);
        log::debug!("{id} => {local:?}");
        Self::compile_expr(expr, instructions);
        instructions.push(Instruction::LocalSet(local_index));

        local
    }

    fn compile_expr(expr: &Expression<'src>, instructions: &mut Vec<Instruction<'src>>) {
        match expr {
            Expression::Number(v) => {
                instructions.push(Instruction::I32Const(*v as i32));
            }
            Expression::Grouping(expr) => Self::compile_expr(expr, instructions),
            Expression::Unary(expr) => Self::compile_expr(expr, instructions), // TODO: Negate number
            Expression::Ref { index, id: _ } => {
                instructions.push(Instruction::LocalGet(*index));
            }
            Expression::Binary { lhs, op, rhs } => {
                Self::compile_expr(lhs, instructions);
                Self::compile_expr(rhs, instructions);

                let ins = match op {
                    Operator::Plus => Instruction::I32Add,
                    Operator::Minus => Instruction::I32Sub,
                    Operator::Mul => Instruction::I32Mul,
                };
                instructions.push(ins);
            }
            other => unimplemented!("{other:?}"),
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
