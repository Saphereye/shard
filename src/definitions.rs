use rand::Rng;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub};
use std::sync::LazyLock;

use crate::bitboards::BitBoard;

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Piece {
    Empty = 0, WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK,
}

#[rustfmt::skip]
pub enum File {
    A = 0, B, C, D, E, F, G, H, None,
}

#[rustfmt::skip]
pub enum Rank {
    R1 = 0, R2, R3, R4, R5, R6, R7, R8, None,
}

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
        let ranks = ['8', '7', '6', '5', '4', '3', '2', '1'];

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
    pub fn new_coor(self, x_offset: isize, y_offset: isize) -> Option<usize> {
        // Extract current row and column from the square index
        let current_row = self as usize / 8;
        let current_col = self as usize % 8;

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

#[rustfmt::skip]
pub enum Castling {
    WK = 0b0001,
    WQ = 0b0010,
    BK = 0b0100,
    BQ = 0b1000,
}

#[derive(Debug)]
pub struct Undo {
    pub move_: u32,
    pub en_passant_square: Square,
    pub fifty_move_counter: u32,
    pub castle_permission: u32,
    pub position_key: u64,
}

pub struct Move {
    /// The move is stored in a 28-bit integer with the following layout:
    /// ```
    ///  0000 0000 0000 0000 0000 0000 0000
    ///     │ │  │ │││   ││       ││      │
    ///     │ └─┬┘ ││└─┬─┘└───┬───┘└───┬──┘
    /// Castle  │  ││  │      │        │
    ///         │  ││  │     To      From
    ///         │  ││  │
    ///         │  ││  Captured
    ///         │  │En Passant
    ///         │  Pawn Start
    ///         Promoted Piece(QBRK)
    /// ```
    pub move_: u32,
    pub score: i32,
}

impl Move {
    pub fn get_from(&self) -> u32 {
        self.move_ & 0x7F
    }

    pub fn get_to(&self) -> u32 {
        (self.move_ >> 7) & 0x7F
    }

    pub fn get_is_captured(&self) -> u32 {
        (self.move_ >> 14) & 0xF
    }

    pub fn get_is_en_passant(&self) -> u32 {
        (self.move_ >> 18) & 0x1
    }

    pub fn get_pawn_start(&self) -> u32 {
        (self.move_ >> 19) & 0x1
    }

    pub fn get_promoted_piece(&self) -> u32 {
        (self.move_ >> 20) & 0x7
    }

    pub fn get_is_castle(&self) -> u32 {
        (self.move_ >> 23) & 0xF
    }
}

static ZOBRIST_TABLE: LazyLock<[[u64; 64]; 14]> = std::sync::LazyLock::new(|| {
    let mut rng = rand::thread_rng();
    let mut table = [[0u64; 64]; 14];

    for piece_table in &mut table {
        for square_hash in piece_table.iter_mut().take(64) {
            // Generate a random u64 value for each side
            *square_hash ^= rng.gen::<u64>();
        }
    }

    table
});

#[rustfmt::skip]
fn is_piece_big(piece: &Piece) -> bool {[false, false, true, true, true, true, true, false, true, true, true, true, true,][*piece as usize]}
#[rustfmt::skip]
fn is_piece_major(piece: &Piece) -> bool {[false, false, false, false, true, true, true, false, false, false, true, true, true,][*piece as usize]}
#[rustfmt::skip]
fn is_piece_minor(piece: &Piece) -> bool {[false, false, true, true, false, false, false, false, true, true, false, false, false,][*piece as usize]}
#[rustfmt::skip]
fn piece_value(piece: &Piece) -> u32 {[0, 100, 320, 330, 500, 900, 20000, 100, 320, 330, 500, 900, 20000,][*piece as usize]}
const KING_MOVE_DIRECTION: [i32; 8] = [-1, -10, 1, 10, -9, -11, 9, 11];
const BISHOP_MOVE_DIRECTION: [i32; 4] = [-9, -11, 11, 9];
const ROOK_MOVE_DIRECTION: [i32; 4] = [-1, -10, 1, 10];
const KNIGHT_MOVE_DIRECTION: [i32; 8] = [-8, -19, -21, -12, 8, 19, 21, 12];

//  0, WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK,

#[rustfmt::skip]
fn is_piece_knight(piece: &Piece) -> bool {[false, false, true, false, false, false, false, false, true, false, false, false, false,][*piece as usize]}
#[rustfmt::skip]
fn is_piece_king(piece: &Piece) -> bool {[false, false, false, false, false, false, true, false, false, false, false, false, true,][*piece as usize]}
#[rustfmt::skip]
fn is_piece_rook_or_queen(piece: &Piece) -> bool {[false, false, false, false, true, true, false, false, false, false, true, true, false,][*piece as usize]}
#[rustfmt::skip]
fn is_piece_bishop_or_queen(piece: &Piece) -> bool {[false, false, false, true, false, true, false, false, false, true, false, true, false,][*piece as usize]}
#[rustfmt::skip]
fn piece_color(piece: &Piece) -> Color {[Color::Both, Color::White, Color::White, Color::White, Color::White, Color::White, Color::White, Color::Black, Color::Black, Color::Black, Color::Black, Color::Black, Color::Black,][*piece as usize]}

#[derive(Debug)]
pub struct Board {
    /// Array of 64 squares
    pub pieces: [Piece; 64],
    /// Bitboards for each piece type: White, Black, Both
    pub pawn_bitboards: [BitBoard; 3],
    /// Squares of the kings for: White, Black
    pub king_square: [Square; 2],
    pub side_to_move: Color,
    pub en_passant_square: Square,
    pub fifty_move_counter: u32,
    /// Half moves
    pub ply: u32,
    /// Half moves in the whole game
    pub history_ply: u32,
    /// 4 bits for each castling permission, as defines in [Castling]
    pub castle_permission: u32,
    /// Zobrist key
    pub position_key: u64,
    /// Number of pieces on the board for each piece type
    pub piece_count: [u32; 13],
    /// Total number of pieces on the board
    pub piece_count_total: u32,
    /// List of pieces on the board for each piece type,
    /// e.g. to add knights to the board,
    /// we can do pieces\[WN\]\[0\] = E1 then piece_list\[WN\]\[1\] = E4
    pub piece_list: [[Square; 10]; 13],
    /// Number of non-pawn pieces on the board. White, Black, Both
    pub big_piece_count: [u32; 3],
    /// Number of rooks and queens on the board. White, Black, Both
    pub major_piece_count: [u32; 3],
    /// Number of bishops and knights on the board. White, Black, Both
    pub minor_piece_count: [u32; 3],
    /// History of the game
    pub history: Vec<Undo>,
    /// Material scores for White, Black, Both
    pub material_scores: [u32; 3],
}

#[rustfmt::skip]
macro_rules! impl_type_for_board {
    ($type:ty) => {
        impl From<$type> for Board {
            fn from(fen: $type) -> Self {
                let mut board = Board::empty();
                let parts: Vec<&str> = fen.split_whitespace().collect();

                // Parse pieces placement
                let mut square_index = 0;
                for ch in parts[0].chars() {
                    match ch {
                        'r' => {
                            board.pieces[square_index] = Piece::BR;

                        }
                        'n' => {
                            board.pieces[square_index] = Piece::BN;
                        }
                        'b' => {
                            board.pieces[square_index] = Piece::BB;
                        }
                        'q' => {
                            board.pieces[square_index] = Piece::BQ;
                        }
                        'k' => {
                            board.pieces[square_index] = Piece::BK;
                        }
                        'p' => {
                            board.pieces[square_index] = Piece::BP;
                        }
                        'R' => {
                            board.pieces[square_index] = Piece::WR;
                        }
                        'N' => {
                            board.pieces[square_index] = Piece::WN;
                        }
                        'B' => {
                            board.pieces[square_index] = Piece::WB;
                        }
                        'Q' => {
                            board.pieces[square_index] = Piece::WQ;
                        }
                        'K' => {
                            board.pieces[square_index] = Piece::WK;
                        }
                        'P' => {
                            board.pieces[square_index] = Piece::WP;
                        }
                        '/' => continue, // Move to next rank
                        digit if digit.is_digit(10) => {
                            // Skip squares based on number
                            let empty_spaces = digit.to_digit(10).unwrap();
                            square_index += empty_spaces as usize;
                            continue;
                        }
                        _ => {}
                    }
                    square_index += 1;
                }

                // Parse side to move
                assert!(parts[1] == "w" || parts[1] == "b");
                board.side_to_move = if parts[1] == "w" {
                    Color::White
                } else {
                    Color::Black
                };

                // Parse castling rights
                board.castle_permission = 0;
                for character in parts[2].chars() {
                    match character {
                        'K' => board.castle_permission |= Castling::WK as u32,
                        'Q' => board.castle_permission |= Castling::WQ as u32,
                        'k' => board.castle_permission |= Castling::BK as u32,
                        'q' => board.castle_permission |= Castling::BQ as u32,
                        _ => {}
                    }
                }
                assert!(board.castle_permission <= 15);

                // Parse en passant square
                board.en_passant_square = match parts[3] {
                    "-" => Square::None,
                    square => {
                        let file = match square.chars().nth(0).unwrap() {
                            'a' => 0, 'b' => 1, 'c' => 2, 'd' => 3, 'e' => 4, 'f' => 5, 'g' => 6, 'h' => 7,
                            _ => 0
                        };
                        let rank = square.chars().nth(1).unwrap().to_digit(10).unwrap() - 1;
                        Square::from((rank * 8 + file) as u8)
                    }
                };

                // Parse halfmove clock and fullmove number (not used in this implementation)
                board.fifty_move_counter = parts[4].parse::<u32>().unwrap_or(0);
                board.ply = parts[5].parse::<u32>().unwrap_or(0) * 2; // FIXME: This might be wrong
                board.history_ply = board.ply; // FIXME: This might be wrong

                board.position_key = board.get_hash();
                board.update_piece_metadata();
                board
            }
        }
    };
}

impl_type_for_board!(String);
impl_type_for_board!(&str);

impl Default for Board {
    fn default() -> Self {
        let mut pieces = [Piece::Empty; 64];

        // Place white pieces
        pieces[0] = Piece::WR;
        pieces[1] = Piece::WN;
        pieces[2] = Piece::WB;
        pieces[3] = Piece::WQ;
        pieces[4] = Piece::WK;
        pieces[5] = Piece::WB;
        pieces[6] = Piece::WN;
        pieces[7] = Piece::WR;
        for piece in pieces.iter_mut().take(16).skip(8) {
            *piece = Piece::WP; // White Pawns
        }

        // Place black pieces
        pieces[56] = Piece::BR;
        pieces[57] = Piece::BN;
        pieces[58] = Piece::BB;
        pieces[59] = Piece::BQ;
        pieces[60] = Piece::BK;
        pieces[61] = Piece::BB;
        pieces[62] = Piece::BN;
        pieces[63] = Piece::BR;
        for piece in pieces.iter_mut().take(56).skip(48) {
            *piece = Piece::BP; // Black Pawns
        }

        let mut temp = Board {
            pieces,
            pawn_bitboards: [BitBoard(0); 3], // FIXME: Initialize pawn bitboards
            king_square: [Square::E1, Square::E8], // Standard starting positions for kings
            side_to_move: Color::White,       // White to move
            en_passant_square: Square::None,  // No en passant at start
            fifty_move_counter: 0,            // Initial counter
            ply: 0,                           // Initial ply
            history_ply: 0,                   // Initial history ply
            castle_permission: 0b1111,        // Both sides can castle both ways
            position_key: 0,                  // Initial Zobrist key
            piece_count: [0; 13],             // Update counts after initializing pieces
            piece_count_total: 0,             // Update total count after initializing pieces
            piece_list: [[Square::None; 10]; 13], // Initialize empty piece lists
            big_piece_count: [0; 3],          // Big piece counts initialized to 0
            major_piece_count: [0; 3],        // Major piece counts initialized to 0
            minor_piece_count: [0; 3],        // Minor piece counts initialized to 0
            history: Vec::new(),              // Empty history
            material_scores: [0; 3],          // Material scores initialized to 0
        };

        temp.position_key = temp.get_hash();
        temp.update_piece_metadata(); // Update piece metadata
        temp
    }
}

impl fmt::Display for Board {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Print the board row by row
        for rank in (0..8).rev() { // Print from rank 8 to 1
            write!(f, "{} ", rank + 1)?; // Print the rank number
            for file in 0..8 {
                let square_index = rank * 8 + file;
                let piece = self.pieces[square_index];
                let piece_char = match piece {
                    Piece::WR => '♖', Piece::WN => '♘', Piece::WB => '♗', Piece::WQ => '♕', Piece::WK => '♔', Piece::WP => '♙',
                    Piece::BR => '♜', Piece::BN => '♞', Piece::BB => '♝', Piece::BQ => '♛', Piece::BK => '♚', Piece::BP => '♟',
                    Piece::Empty => '.',
                };
                write!(f, "{} ", piece_char)?; // Print the piece character
            }
            writeln!(f)?; // Move to the next line
        }
        writeln!(f, "  a b c d e f g h")?; // Print the file labels
        writeln!(f, "Side to move: {}", self.side_to_move)?;
        writeln!(f, "En passant square: {}", self.en_passant_square)?;
        writeln!(f, "Castle permission: {:#06b}", self.castle_permission)?;
        writeln!(f, "Fifty move counter: {}", self.fifty_move_counter)?;
        writeln!(f, "Ply: {}", self.ply)?;
        writeln!(f, "History ply: {}", self.history_ply)?;
        writeln!(f, "Position key: {:016X}", self.position_key)?;
        writeln!(f, "Piece count: {:?}", self.piece_count_total)?;
        Ok(())
    }
}

impl Hash for Board {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.position_key);
    }
}

impl Board {
    pub fn new<T>(value: T) -> Self
    where
        T: Into<Self>,
    {
        let mut temp = value.into();
        temp.update_piece_metadata();
        temp
    }

    pub fn empty() -> Self {
        let mut temp = Board {
            pieces: [Piece::Empty; 64],
            pawn_bitboards: [BitBoard(0); 3],
            king_square: [Square::None, Square::None],
            side_to_move: Color::White,
            en_passant_square: Square::None,
            fifty_move_counter: 0,
            ply: 0,
            history_ply: 0,
            castle_permission: 0b1111,
            position_key: 0,
            piece_count: [0; 13],
            piece_count_total: 0,
            piece_list: [[Square::None; 10]; 13],
            big_piece_count: [0; 3],
            major_piece_count: [0; 3],
            minor_piece_count: [0; 3],
            history: Vec::new(),
            material_scores: [0; 3],
        };

        temp.position_key = temp.get_hash();
        temp
    }

    pub fn get_hash(&self) -> u64 {
        let mut hash = 0;

        for square_index in 0..64 {
            let piece = self.pieces[square_index];
            if piece != Piece::Empty {
                hash ^= ZOBRIST_TABLE[piece as usize][square_index];
            }
        }

        if self.side_to_move == Color::Black {
            hash ^= ZOBRIST_TABLE[12][0];
        }

        if self.en_passant_square != Square::None {
            hash ^= ZOBRIST_TABLE[12][self.en_passant_square as usize];
        }

        hash ^= ZOBRIST_TABLE[13][self.castle_permission as usize];

        hash
    }

    pub fn reset(&mut self) {
        *self = Board::default();
    }

    pub fn update_piece_metadata(&mut self) {
        self.piece_count = [0; 13];
        self.piece_count_total = 0;
        self.big_piece_count = [0; 3];
        self.major_piece_count = [0; 3];
        self.minor_piece_count = [0; 3];
        self.material_scores = [0; 3];

        for (index, piece) in self.pieces.iter().enumerate() {
            if *piece == Piece::Empty {
                continue;
            }

            let square: Square = index.into();

            if is_piece_big(piece) {
                self.big_piece_count[piece_color(piece) as usize] += 1;
            } else if is_piece_major(piece) {
                self.major_piece_count[piece_color(piece) as usize] += 1;
            } else if is_piece_minor(piece) {
                self.minor_piece_count[piece_color(piece) as usize] += 1;
            }

            self.material_scores[piece_color(piece) as usize] += piece_value(piece);
            self.piece_list[*piece as usize][self.piece_count[*piece as usize] as usize] = square;
            self.piece_count[*piece as usize] += 1;
            self.piece_count_total += 1;

            match *piece {
                Piece::WK => self.king_square[Color::White as usize] = square,
                Piece::BK => self.king_square[Color::Black as usize] = square,
                Piece::WP => {
                    self.pawn_bitboards[Color::White as usize].set(square);
                    self.pawn_bitboards[Color::Both as usize].set(square);
                }
                Piece::BP => {
                    self.pawn_bitboards[Color::Black as usize].set(square);
                    self.pawn_bitboards[Color::Both as usize].set(square);
                }
                _ => (),
            }
        }
    }

    fn get_piece(&self, square: Square) -> Piece {
        self.pieces[square as usize]
    }

    fn get_piece_at_index(&self, index: usize) -> Piece {
        self.pieces[index]
    }

    /// Get the piece at a square with an offset.
    /// TIP: The board has its origin at top-left corner.
    fn get_piece_with_offset(&self, square: Square, x_offset: isize, y_offset: isize) -> Piece {
        let square_x_corr: isize = square as isize % 8;
        let square_y_corr: isize = square as isize / 8;

        if !((0..8).contains(&(square_x_corr + x_offset))
            && (0..8).contains(&(square_y_corr + y_offset)))
        {
            return Piece::Empty;
        }

        self.pieces[(square as isize + x_offset + y_offset * 8) as usize]
    }

    pub fn is_square_attacked(&self, square: Square) -> bool {
        // Pawns
        if self.side_to_move == Color::White {
            if self.get_piece_with_offset(square, -1, -1) == Piece::WP
                || self.get_piece_with_offset(square, 1, -1) == Piece::WP
            {
                return true;
            }
        } else if self.get_piece_with_offset(square, -1, 1) == Piece::BP
            || self.get_piece_with_offset(square, 1, 1) == Piece::BP
        {
            return true;
        }

        // Knights
        #[rustfmt::skip]
        let knight_offsets: [(isize, isize); 8] = [
            (-1, -2), (1, -2), (-2, -1), (2, -1),
            (-2,  1), (2,  1), (-1,  2), (1,  2),
        ];

        let knight_piece = if self.side_to_move == Color::White {
            Piece::WN
        } else {
            Piece::BN
        };

        for (x_offset, y_offset) in knight_offsets.iter() {
            if self.get_piece_with_offset(square, *x_offset, *y_offset) == knight_piece {
                return true;
            }
        }

        // Kings
        #[rustfmt::skip]
        let king_offsets: [(isize, isize); 8] = [
            (-1, -1), (0, -1), (1, -1),
            (-1,  0),          (1,  0),
            (-1,  1), (0,  1), (1,  1),
        ];

        let king_piece = if self.side_to_move == Color::White {
            Piece::WK
        } else {
            Piece::BK
        };

        for (x_offset, y_offset) in king_offsets.iter() {
            if self.get_piece_with_offset(square, *x_offset, *y_offset) == king_piece {
                return true;
            }
        }

        // Rooks and Queens (horizontal and vertical lines)
        let rook_offsets: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

        let rook_piece = if self.side_to_move == Color::White {
            Piece::WR
        } else {
            Piece::BR
        };

        let queen_piece = if self.side_to_move == Color::White {
            Piece::WQ
        } else {
            Piece::BQ
        };

        for (x_offset, y_offset) in rook_offsets.iter() {
            let mut step = 1;
            while let Some(index) = square.new_coor(x_offset * step, y_offset * step) {
                let piece = self.get_piece_at_index(index);
                if piece == rook_piece || piece == queen_piece {
                    return true;
                } else if piece != Piece::Empty {
                    break;
                }
                step += 1;
            }
        }

        // Bishops and Queens (diagonals)
        let bishop_offsets: [(isize, isize); 4] = [(-1, -1), (1, -1), (-1, 1), (1, 1)];

        let bishop_piece = if self.side_to_move == Color::White {
            Piece::WB
        } else {
            Piece::BB
        };

        for (x_offset, y_offset) in bishop_offsets.iter() {
            let mut step = 1;
            while let Some(index) = square.new_coor(x_offset * step, y_offset * step) {
                let piece = self.get_piece_at_index(index);
                if piece == bishop_piece || piece == queen_piece {
                    return true;
                } else if piece != Piece::Empty {
                    break;
                }
                step += 1;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const FEN1: &str = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2";
    const FEN2: &str = "8/2P5/4B3/7p/1N1Q1p1P/ppK1Pk2/bR1r3P/8 w - - 0 1";

    #[test]
    fn check_pawn_bitboard() {
        let board: Board = Board::new(FEN1);
        assert_eq!(
            board.pawn_bitboards[Color::White as usize],
            BitBoard(67272588153323520)
        );
        assert_eq!(
            board.pawn_bitboards[Color::Black as usize],
            BitBoard(67173120)
        );
        assert_eq!(
            board.pawn_bitboards[Color::Both as usize],
            BitBoard(67272588220496640)
        );
    }

    #[test]
    fn check_piece_count() {
        let board: Board = Board::new(FEN1);
        assert_eq!(board.piece_count[Piece::WP as usize], 8);
        assert_eq!(board.piece_count[Piece::BP as usize], 8);
    }

    #[test]
    fn check_correct_square_attacked() {
        let board: Board = Board::new(FEN2);
        assert_eq!(board.is_square_attacked(Square::B5), true);
        assert_eq!(board.is_square_attacked(Square::E3), false);
    }
}
