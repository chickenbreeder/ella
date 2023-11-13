use crate::syntax::stmt::{FnDecl, FnType};

pub(crate) fn visit_native_fn(decl: &FnDecl) {
    let (params, body) = match &decl.ty {
        FnType::ForeignFn { func } => panic!("Cannot generate code for a native function"),
        FnType::NativeFn { params, body } => (params, body),
    };

    todo!("Actually generate code :)");
}
