use std::{iter::Peekable, str::CharIndices};

use crate::token::{OperatorKind, Token};

pub(crate) struct Lexer<'src> {
    src: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            None => None,
            Some((_, '+')) => Some(Token::Op(OperatorKind::Plus)),
            Some((_, '-')) => Some(Token::Op(OperatorKind::Minus)),
            Some((_, '*')) => Some(Token::Op(OperatorKind::Mul)),
            Some((_, '(')) => Some(Token::LParen),
            Some((_, ')')) => Some(Token::RParen),
            Some((_, '[')) => Some(Token::LBracket),
            Some((_, ']')) => Some(Token::RBracket),
            Some((off, c)) => {
                if c.is_whitespace() {
                    return self.next();
                }
                if c.is_digit(10) {
                    return Some(self.read_number(off));
                }
                if Self::is_id_start(c) {
                    return Some(self.read_id(off));
                }

                Some(Token::Invalid(c))
            }
        }
    }
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            chars: src.char_indices().peekable(),
        }
    }

    #[inline]
    fn bump(&mut self) {
        let _ = self.chars.next();
    }

    fn slice_until<P>(&mut self, from_off: usize, predicate: P) -> &'src str
    where
        P: Fn(char) -> bool,
    {
        while let Some(&(off, c)) = self.chars.peek() {
            if predicate(c) {
                return &self.src[from_off..off];
            }
            self.bump();
        }
        &self.src[from_off..self.src.len()]
    }

    fn read_number(&mut self, from_off: usize) -> Token<'src> {
        let s = self.slice_until(from_off, |c| !c.is_digit(10));
        Token::Number(
            s.parse::<i64>()
                .expect("Failed to parse number. (This should never happen)"),
        )
    }

    fn read_id(&mut self, from_off: usize) -> Token<'src> {
        let s = self.slice_until(from_off, |c| !Self::is_id_part(c));
        Token::Id(s)
    }

    fn is_id_start(c: char) -> bool {
        match c {
            c if c >= 'a' && c <= 'z' => true,
            c if c >= 'A' && c <= 'Z' => true,
            '_' => true,
            _ => false,
        }
    }

    fn is_id_part(c: char) -> bool {
        Self::is_id_start(c) || c.is_digit(10)
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;
    use crate::token::Token;

    fn tokenize_str(s: &str) -> Vec<Token> {
        Lexer::new(s).into_iter().collect()
    }

    #[test]
    fn read_number() {
        let tokens = tokenize_str("48$7 1024 \n9\n8");
        let expected = &[
            Token::Number(48),
            Token::Invalid('$'),
            Token::Number(7),
            Token::Number(1024),
            Token::Number(9),
            Token::Number(8),
        ];

        assert_eq!(tokens, expected);
    }
}