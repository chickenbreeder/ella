#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Assoc {
    Left,
}

impl Operator {
    pub fn get_precedence(self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 10,
            Self::Mul => 20,
        }
    }

    pub fn get_assoc(self) -> Assoc {
        match self {
            Self::Plus | Self::Minus | Self::Mul => Assoc::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Keyword {
    Let,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'src> {
    Invalid(char),
    Number(i64),
    Op(Operator),
    Kw(Keyword),
    Id(&'src str),

    LParen,
    RParen,
    LBracket,
    RBracket,
}
