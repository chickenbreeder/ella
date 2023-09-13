#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Value {
    None,
    Number(i64),
    Boolean(bool),
}
