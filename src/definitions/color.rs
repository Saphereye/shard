use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Color {
    White = 0,
    Black,
    Both,
}

impl Display for Color {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Color::White => write!(f, "White"),
            Color::Black => write!(f, "Black"),
            Color::Both => write!(f, "Both"),
        }
    }
}
