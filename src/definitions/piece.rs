use std::fmt::{self, Display, Formatter};

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Piece {
    Empty = 0, WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK,
}

impl Display for Piece {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Piece::Empty => write!(f, "."),
            Piece::WP => write!(f, "P"),
            Piece::WN => write!(f, "N"),
            Piece::WB => write!(f, "B"),
            Piece::WR => write!(f, "R"),
            Piece::WQ => write!(f, "Q"),
            Piece::WK => write!(f, "K"),
            Piece::BP => write!(f, "p"),
            Piece::BN => write!(f, "n"),
            Piece::BB => write!(f, "b"),
            Piece::BR => write!(f, "r"),
            Piece::BQ => write!(f, "q"),
            Piece::BK => write!(f, "k"),
        }
    }
}

#[rustfmt::skip]
macro_rules! impl_type_for_piece {
    ($type:ty) => {
        impl From<$type> for Piece {
            fn from(value: $type) -> Self {
                match value {
                    0..=12 => unsafe { std::mem::transmute::<u8, Piece>(value as u8) },
                    _ => Piece::Empty,
                }
            }
        }
    };
}

impl_type_for_piece!(u32);
