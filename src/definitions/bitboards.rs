use std::fmt::{self, Display, Formatter};

use crate::definitions::square::Square;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct BitBoard(pub u64);

impl BitBoard {
    // Create a new empty BitBoard
    pub fn new(board_state: u64) -> Self {
        BitBoard(board_state)
    }

    // Set a bit at the given square index
    pub fn set(&mut self, square: Square) {
        self.0 |= 1 << square as u64;
    }

    // Clear a bit at the given square index
    pub fn clear(&mut self, square: Square) {
        self.0 &= !(1 << square as u64);
    }

    // Check if a bit is set at the given square index
    pub fn get(&self, square: Square) -> bool {
        (self.0 >> square as u64) & 1 == 1
    }

    pub fn pop(&mut self) -> Square {
        // Find the least significant bit that is set
        let square = self.0.trailing_zeros() as u8;

        // Unset the bit
        self.0 &= !(1 << square);

        // Return the square
        square.into()
    }

    pub fn count(&self) -> u32 {
        self.0.count_ones()
    }
}

impl Display for BitBoard {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let files = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        let ranks = ['8', '7', '6', '5', '4', '3', '2', '1'];

        for (rank_index, rank) in ranks.iter().enumerate() {
            write!(f, "{} ", rank)?; // Print rank label
            for (file_index, _) in files.iter().enumerate() {
                let square = (7 - rank_index) * 8 + file_index;
                if self.get(square.into()) {
                    write!(f, "X ")?;
                } else {
                    write!(f, ". ")?;
                }
            }
            writeln!(f)?;
        }
        write!(f, "  ")?;
        for file in files.iter() {
            write!(f, "{} ", file)?;
        }
        writeln!(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitboard() {
        let mut board = BitBoard::default();
        board.set(Square::D2);
        board.set(Square::D3);
        board.set(Square::D4);
        assert_eq!(board, BitBoard(134744064));
        assert_eq!(board.count(), 3);
        assert_eq!(board.pop(), Square::D2);
        assert_eq!(board.count(), 2);
        assert_eq!(board.pop(), Square::D3);
        assert_eq!(board.count(), 1);
        assert_eq!(board.pop(), Square::D4);
        assert_eq!(board.count(), 0);
    }
}
