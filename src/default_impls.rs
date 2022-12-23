use std::fmt::Display;

use crate::lgraph::{Bracket, Token};

impl Token for char {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefaultBracket {
    RoundOpen(usize),
    RoundClose(usize),
    AngleOpen(usize),
    AngleClose(usize),
}

impl Display for DefaultBracket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Bracket for DefaultBracket {
    fn is_opening(&self) -> bool {
        matches!(
            self,
            DefaultBracket::AngleOpen(_) | DefaultBracket::RoundOpen(_)
        )
    }

    fn index(&self) -> usize {
        match self {
            DefaultBracket::RoundOpen(i) => *i,
            DefaultBracket::RoundClose(i) => *i,
            DefaultBracket::AngleOpen(i) => *i,
            DefaultBracket::AngleClose(i) => *i,
        }
    }
}
