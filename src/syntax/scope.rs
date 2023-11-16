use std::collections::HashMap;

use crate::error::{ErrorKind, PResult};

use super::LocalIndex;

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct ScopeEnv<'src> {
    offset: LocalIndex,
    locals: HashMap<&'src str, LocalIndex>,
}

impl<'src> ScopeEnv<'src> {
    pub fn with_offset(offset: LocalIndex) -> Self {
        Self {
            offset,
            locals: HashMap::new(),
        }
    }

    pub fn with_locals(locals: HashMap<&'src str, LocalIndex>) -> Self {
        Self {
            offset: locals.len() as u32,
            locals,
        }
    }

    pub fn new() -> Self {
        Self::with_offset(0)
    }

    pub fn add(&mut self, id: &'src str) -> PResult<LocalIndex> {
        if self.locals.get(id).is_some() {
            return Err(ErrorKind::ParseError(format!(
                "{id} is already defined in current scope"
            )));
        }

        let idx = self.offset;
        self.offset += 1;

        self.locals.insert(id, idx);
        Ok(idx)
    }

    pub fn get(&self, id: &'src str) -> PResult<LocalIndex> {
        match self.locals.get(id) {
            Some(idx) => Ok(*idx),
            None => Err(ErrorKind::ParseError(format!("Undefined reference `{id}`"))),
        }
    }

    pub fn locals(&self) -> &HashMap<&'src str, LocalIndex> {
        &self.locals
    }
}
