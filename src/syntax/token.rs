use wasm_encoder::ValType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Mul,

    Lt,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Assoc {
    Left,
}

pub(crate) type Precedence = u8;

impl Operator {
    pub fn precedence(self) -> Precedence {
        match self {
            Self::Lt | Self::Gt => 5,
            Self::Plus | Self::Minus => 10,
            Self::Mul => 20,
        }
    }

    pub fn assoc(self) -> Assoc {
        match self {
            Self::Lt | Self::Gt => Assoc::Left,
            Self::Plus | Self::Minus | Self::Mul => Assoc::Left,
        }
    }

    pub fn get(self) -> (Precedence, Assoc) {
        (self.precedence(), self.assoc())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Keyword {
    Let,
    Fn,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum Type {
    Void,
    I32,
    I64,
}

impl Into<Option<ValType>> for Type {
    fn into(self) -> Option<ValType> {
        match self {
            Self::Void => None,
            Self::I32 => Some(ValType::I32),
            Self::I64 => Some(ValType::I64),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'src> {
    Invalid(char),
    Number(i64),
    Op(Operator),
    Kw(Keyword),
    Ty(Type),
    Id(&'src str),

    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,

    Eq,

    Comma,
    Semicolon,
    Colon,
}
