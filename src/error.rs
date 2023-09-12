#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum ErrorKind {
    ParseError(String),
    RuntimeError(String),
}

pub(crate) type PResult<T> = Result<T, ErrorKind>;
