use std::collections::HashMap;

pub(super) type Environment<'src> = HashMap<&'src str, i64>;
