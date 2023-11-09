use std::collections::HashMap;

use super::value::Value;

pub(crate) struct Environment<'src> {
    entries: HashMap<&'src str, Value>,
    ret_val: Option<Value>,
}

impl<'src> Environment<'src> {
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            ret_val: None,
        }
    }

    pub fn get(&self, k: &'src str) -> Option<&Value> {
        self.entries.get(k)
    }

    pub fn get_mut(&mut self, k: &'src str) -> Option<&mut Value> {
        self.entries.get_mut(k)
    }

    pub fn insert(&mut self, k: &'src str, v: Value) {
        let _ = self.entries.insert(k, v);
    }

    pub fn contains_key(&self, k: &'src str) -> bool {
        self.entries.contains_key(k)
    }

    pub fn get_ret_val(&self) -> Option<Value> {
        self.ret_val
    }

    pub fn set_ret_val(&mut self, v: Value) {
        self.ret_val = Some(v);
    }
}
