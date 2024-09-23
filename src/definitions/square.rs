use crate::definitions::file::File;
use crate::definitions::rank::Rank;
use std::fmt::{self, Display, Formatter};
use std::ops::{Add, Sub};

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Square {
    A1 = 0, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8, None,
}

impl Display for Square {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        let ranks = ['1', '2', '3', '4', '5', '6', '7', '8'];

        match self {
            Square::None => write!(f, "None"),
            square => write!(
                f,
                "{}{}",
                files[*square as usize % 8],
                ranks[*square as usize / 8]
            ),
        }
    }
}

impl Square {
    pub fn new_coor(&self, x_offset: isize, y_offset: isize) -> Option<usize> {
        // Extract current row and column from the square index
        let current_row = *self as usize / 8;
        let current_col = *self as usize % 8;

        // Calculate new row and column based on offsets
        let new_row = current_row as isize + y_offset;
        let new_col = current_col as isize + x_offset;

        // Check if the new row and column are within bounds
        if (0..8).contains(&new_row) && (0..8).contains(&new_col) {
            Some((new_row * 8 + new_col) as usize)
        } else {
            None
        }
    }

    pub fn new_square(&self, x_offset: isize, y_offset: isize) -> Square {
        match self.new_coor(x_offset, y_offset) {
            Some(index) => unsafe { std::mem::transmute::<u8, Square>(index as u8) },
            None => Square::None,
        }
    }

    #[rustfmt::skip]
    pub fn get_rank(&self) -> Rank {
        match self {
            Square::A1 | Square::B1 | Square::C1 | Square::D1 | Square::E1 | Square::F1 | Square::G1 | Square::H1 => Rank::R1,
            Square::A2 | Square::B2 | Square::C2 | Square::D2 | Square::E2 | Square::F2 | Square::G2 | Square::H2 => Rank::R2,
            Square::A3 | Square::B3 | Square::C3 | Square::D3 | Square::E3 | Square::F3 | Square::G3 | Square::H3 => Rank::R3,
            Square::A4 | Square::B4 | Square::C4 | Square::D4 | Square::E4 | Square::F4 | Square::G4 | Square::H4 => Rank::R4,
            Square::A5 | Square::B5 | Square::C5 | Square::D5 | Square::E5 | Square::F5 | Square::G5 | Square::H5 => Rank::R5,
            Square::A6 | Square::B6 | Square::C6 | Square::D6 | Square::E6 | Square::F6 | Square::G6 | Square::H6 => Rank::R6,
            Square::A7 | Square::B7 | Square::C7 | Square::D7 | Square::E7 | Square::F7 | Square::G7 | Square::H7 => Rank::R7,
            Square::A8 | Square::B8 | Square::C8 | Square::D8 | Square::E8 | Square::F8 | Square::G8 | Square::H8 => Rank::R8,
            _ => Rank::None,
        }
    }

    #[rustfmt::skip]
    pub fn get_file(&self) -> File {
        match self {
            Square::A1 | Square::A2 | Square::A3 | Square::A4 | Square::A5 | Square::A6 | Square::A7 | Square::A8 => File::A,
            Square::B1 | Square::B2 | Square::B3 | Square::B4 | Square::B5 | Square::B6 | Square::B7 | Square::B8 => File::B,
            Square::C1 | Square::C2 | Square::C3 | Square::C4 | Square::C5 | Square::C6 | Square::C7 | Square::C8 => File::C,
            Square::D1 | Square::D2 | Square::D3 | Square::D4 | Square::D5 | Square::D6 | Square::D7 | Square::D8 => File::D,
            Square::E1 | Square::E2 | Square::E3 | Square::E4 | Square::E5 | Square::E6 | Square::E7 | Square::E8 => File::E,
            Square::F1 | Square::F2 | Square::F3 | Square::F4 | Square::F5 | Square::F6 | Square::F7 | Square::F8 => File::F,
            Square::G1 | Square::G2 | Square::G3 | Square::G4 | Square::G5 | Square::G6 | Square::G7 | Square::G8 => File::G,
            Square::H1 | Square::H2 | Square::H3 | Square::H4 | Square::H5 | Square::H6 | Square::H7 | Square::H8 => File::H,
            _ => File::None,
        }
    }
}

#[rustfmt::skip]
macro_rules! impl_type_for_square {
    ($type:ty) => {
        impl From<$type> for Square {
            fn from(value: $type) -> Self {
                match value {
                    0..=63 => unsafe { std::mem::transmute::<u8, Square>(value as u8) },
                    _ => Square::None,
                }
            }
        }

        impl Add<$type> for Square {
            type Output = Square;

            fn add(self, rhs: $type) -> Square {
                let result: $type = self as $type + rhs;
                match result {
                    0..=63 => unsafe { std::mem::transmute::<u8, Square>(result as u8) },
                    _ => Square::None,
                }
            }
        }

        impl Sub<$type> for Square {
            type Output = Square;

            fn sub(self, rhs: $type) -> Square {
                let result: $type = self as $type - rhs;
                match result {
                    0..=63 => unsafe { std::mem::transmute::<u8, Square>(result as u8) },
                    _ => Square::None,
                }
            }
        }
    };
}

impl_type_for_square!(u8);
impl_type_for_square!(usize);
impl_type_for_square!(u64);
