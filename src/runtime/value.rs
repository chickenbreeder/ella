#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    None,
    Number(i64),
    List(Vec<Value>),
    Boolean(bool),
}
