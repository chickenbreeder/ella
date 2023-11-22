use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
    TypeSection, ValType,
};

use crate::{
    error::{ErrorKind, PResult},
    syntax::{
        stmt::{FnType, Statement, TypedId},
        Expression, FunctionIndex, LocalIndex, Operator, Parser, Type,
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

    pub fn compile_to_module(&mut self) -> PResult<Module> {
        let mut module = Module::new();
        let mut code = CodeSection::new();
        let mut types = TypeSection::new();
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();

        while let Some(stmt) = self.parser.parse_top_level_stmt()? {
            match *stmt {
                Statement::FnDecl(decl) => match &decl.ty {
                    FnType::ForeignFn { func: _ } => {
                        return Err(ErrorKind::ParseError(
                            "Cannot compile a foreign function".into(),
                        ))
                    }
                    FnType::NativeFn {
                        index,
                        ty,
                        params,
                        body,
                    } => {
                        let index = *index;

                        exports.export(decl.id, ExportKind::Func, index);
                        functions.function(index);

                        let mut instructions = vec![];
                        Self::compile_fn_signature(params, *ty, &mut types);
                        let locals = Self::compile_fn(body, &mut instructions);
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
            }
        }

        module.section(&types);
        module.section(&functions);
        module.section(&exports);
        module.section(&code);

        Ok(module)
    }

    pub fn compile(&mut self) -> PResult<Vec<u8>> {
        let module = self.compile_to_module()?;
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

    fn compile_fn_signature(params: &[TypedId<'src>], ty: Type, types: &mut TypeSection) {
        let results = match ty.into() {
            Some(ty) => vec![ty],
            None => vec![],
        };
        let params: Vec<ValType> = params.iter().map(|_| ValType::I32).collect();

        types.function(params, results);
    }

    fn compile_stmt(
        stmt: &Statement<'src>,
        locals: &mut Vec<(LocalIndex, ValType)>,
        instructions: &mut Vec<Instruction<'src>>,
    ) {
        match stmt {
            Statement::LetDecl { id, index, value } => {
                let local = Self::compile_let_decl(id, *index, value, instructions);
                locals.push(local);
            }
            Statement::Assignment {
                id: _,
                index,
                value,
            } => {
                Self::compile_expr(value, instructions);
                instructions.push(Instruction::LocalSet(*index));
            }
            Statement::Return(expr) => {
                Self::compile_expr(expr, instructions);
                // ? instructions.push(Instruction::Return);
            }
            Statement::If(expr, _) => {
                Self::compile_expr(expr, instructions);
                log::warn!("The generated code for if statements is not complete yet");
            }
            other => unimplemented!("Statement `{other:?}` is currently not supported"),
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
                    Operator::Lt => Instruction::I32LtS,
                    Operator::Gt => Instruction::I32GtS,
                };
                instructions.push(ins);
            }
            Expression::Call {
                id: _,
                index,
                params,
            } => Self::compile_call_expr(*index, params, instructions),
            other => unimplemented!("{other:?}"),
        };
    }

    fn compile_call_expr(
        index: FunctionIndex,
        params: &[Expression<'src>],
        instructions: &mut Vec<Instruction<'src>>,
    ) {
        for expr in params {
            Self::compile_expr(expr, instructions);
        }

        instructions.push(Instruction::Call(index));
    }
}

#[cfg(test)]
mod test {
    use super::Compiler;

    #[test]
    fn simple() {
        let mut compiler = Compiler::from_src("fn foo(): void {}");
        let bytes = compiler.compile().unwrap();

        assert_eq!(0x00, bytes[0]);
        assert_eq!(0x61, bytes[1]);
        assert_eq!(0x73, bytes[2]);
        assert_eq!(0x6D, bytes[3]);
    }
}
