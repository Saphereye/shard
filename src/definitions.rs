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

#[rustfmt::skip]
pub enum File {
    A = 0, B, C, D, E, F, G, H, None,
}

impl Display for File {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            File::A => write!(f, "a"),
            File::B => write!(f, "b"),
            File::C => write!(f, "c"),
            File::D => write!(f, "d"),
            File::E => write!(f, "e"),
            File::F => write!(f, "f"),
            File::G => write!(f, "g"),
            File::H => write!(f, "h"),
            File::None => write!(f, "None"),
        }
    }
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub captured_piece: Piece,
    pub is_enpassant: bool,
    pub is_pawn_start: bool,
    pub promoted_piece: Piece,
    pub castle: CastleType,
    pub current_piece: Piece,
    pub score: i32,
}

#[derive(Debug)]
pub enum CastleType {
    KingSide,
    QueenSide,
    None,
}

impl Move {
    pub fn new(
        from: Square,
        to: Square,
        captured_piece: Piece, // Piece which got captured
        is_enpassant: bool,
        is_pawn_start: bool,
        promoted_piece: Piece,
        castle: CastleType,
        current_piece: Piece,
    ) -> Self {
        Self {
            from,
            to,
            captured_piece,
            is_enpassant,
            is_pawn_start,
            promoted_piece,
            castle,
            current_piece,
            score: 0,
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut notation = String::new();

        match self.castle {
            CastleType::KingSide => {
                // Handle king-side castling
                notation.push_str("O-O");
            }
            CastleType::QueenSide => {
                // Handle queen-side castling
                notation.push_str("O-O-O");
            }
            CastleType::None => {
                // Handle normal moves and captures

                if self.current_piece != Piece::WP && self.current_piece != Piece::BP {
                    // For non-pawn pieces, add the piece type
                    notation.push(
                        format!("{}", self.current_piece)
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }

                // Handle captures
                if self.captured_piece != Piece::Empty {
                    if self.current_piece == Piece::WP || self.current_piece == Piece::BP {
                        // Pawn captures should include the file of the pawn
                        notation.push(
                            format!("{}", self.from.get_file())
                                .to_lowercase()
                                .chars()
                                .next()
                                .unwrap(),
                        );
                    }
                    notation.push('x');
                }

                // Add the destination square
                notation.push_str(&self.to.to_string());

                // Handle promotion
                if self.promoted_piece != Piece::Empty {
                    notation.push('=');
                    notation.push(
                        format!("{}", self.promoted_piece)
                            .to_uppercase()
                            .chars()
                            .next()
                            .unwrap(),
                    );
                }
            }
        }

        // Write the constructed notation
        write!(f, "{}", notation)
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

#[rustfmt::skip]
const KING_OFFSETS: [(isize, isize); 8] = [
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1,  0),
    (-1,  1), (0,  1), (1,  1),
];

#[rustfmt::skip]
const KNIGHT_OFFSETS: [(isize, isize); 8] = [
    (-1, -2), (1, -2), (-2, -1), (2, -1),
    (-2,  1), (2,  1), (-1,  2), (1,  2),
];

const ROOK_OFFSETS: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

const BISHOP_OFFSETS: [(isize, isize); 4] = [(-1, -1), (1, -1), (-1, 1), (1, 1)];

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
                            board.pieces[flip_board(square_index)] = Piece::BR;
                        }
                        'n' => {
                            board.pieces[flip_board(square_index)] = Piece::BN;
                        }
                        'b' => {
                            board.pieces[flip_board(square_index)] = Piece::BB;
                        }
                        'q' => {
                            board.pieces[flip_board(square_index)] = Piece::BQ;
                        }
                        'k' => {
                            board.pieces[flip_board(square_index)] = Piece::BK;
                        }
                        'p' => {
                            board.pieces[flip_board(square_index)] = Piece::BP;
                        }
                        'R' => {
                            board.pieces[flip_board(square_index)] = Piece::WR;
                        }
                        'N' => {
                            board.pieces[flip_board(square_index)] = Piece::WN;
                        }
                        'B' => {
                            board.pieces[flip_board(square_index)] = Piece::WB;
                        }
                        'Q' => {
                            board.pieces[flip_board(square_index)] = Piece::WQ;
                        }
                        'K' => {
                            board.pieces[flip_board(square_index)] = Piece::WK;
                        }
                        'P' => {
                            board.pieces[flip_board(square_index)] = Piece::WP;
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

                // Parse halfmove clock and fullmove number
                board.fifty_move_counter = parts[4].parse::<u32>().unwrap_or(0);
                board.ply = parts[5].parse::<u32>().unwrap_or(0) * 2;
                board.history_ply = board.ply; // FIXME: This might be wrong

                board.position_key = board.get_hash();
                board.update_piece_metadata();
                board
            }
        }
    };
}

fn flip_board(square_index: usize) -> usize {
    let rank = square_index / 8;
    let file = square_index % 8;
    ((7 - rank) * 8) + (file) // Flip the file by subtracting from 7
}

impl_type_for_board!(String);
impl_type_for_board!(&str);

impl Default for Board {
    fn default() -> Self {
        let mut pieces = [Piece::Empty; 64];

        pieces[0] = Piece::WR;
        pieces[1] = Piece::WN;
        pieces[2] = Piece::WB;
        pieces[3] = Piece::WQ;
        pieces[4] = Piece::WK;
        pieces[5] = Piece::WB;
        pieces[6] = Piece::WN;
        pieces[7] = Piece::WR;
        for piece in pieces.iter_mut().take(16).skip(8) {
            *piece = Piece::WP; // Black Pawns
        }

        pieces[56] = Piece::BR;
        pieces[57] = Piece::BN;
        pieces[58] = Piece::BB;
        pieces[59] = Piece::BQ;
        pieces[60] = Piece::BK;
        pieces[61] = Piece::BB;
        pieces[62] = Piece::BN;
        pieces[63] = Piece::BR;
        for piece in pieces.iter_mut().take(56).skip(48) {
            *piece = Piece::BP; // White Pawns
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
                let square_index = (rank) * 8 + file;
                let piece = self.pieces[square_index];
                let piece_char = match piece {
                    Piece::WR => 'R', Piece::WN => 'N', Piece::WB => 'B', Piece::WQ => 'Q', Piece::WK => 'K', Piece::WP => 'P',
                    Piece::BR => 'r', Piece::BN => 'n', Piece::BB => 'b', Piece::BQ => 'q', Piece::BK => 'k', Piece::BP => 'p',
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
    /// The board is like this
    /// ```
    /// (0, 7)───┐             ┌────(7, 7)
    ///         ┌▼┬─┬─┬─┬─┬─┬─┬▼┐
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         ├─┼─┼─┼─┼─┼─┼─┼─┤
    ///         └▲┴─┴─┴─┴─┴─┴─┴▲┘
    ///  (0, 0)──┘ B C D E F G └───(7, 0)
    ///
    fn get_piece_with_offset(&self, square: Square, x_offset: isize, y_offset: isize) -> Piece {
        let square_x_corr: isize = square as isize % 8;
        let square_y_corr: isize = square as isize / 8;

        if !((0..8).contains(&(square_x_corr + x_offset))
            && (0..8).contains(&(square_y_corr + y_offset)))
        {
            return Piece::Empty;
        }

        self.pieces[(square_x_corr + x_offset + (square_y_corr + y_offset) * 8) as usize]
    }

    // Is this square attacked by the side to move?
    pub fn is_square_attacked(&self, square: Square, side_to_move: Color) -> bool {
        // Pawns
        if side_to_move == Color::White {
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
        let knight_piece = if side_to_move == Color::White {
            Piece::WN
        } else {
            Piece::BN
        };

        for (x_offset, y_offset) in KNIGHT_OFFSETS.iter() {
            if self.get_piece_with_offset(square, *x_offset, *y_offset) == knight_piece {
                return true;
            }
        }

        // Kings
        let king_piece = if side_to_move == Color::White {
            Piece::WK
        } else {
            Piece::BK
        };

        for (x_offset, y_offset) in KING_OFFSETS.iter() {
            if self.get_piece_with_offset(square, *x_offset, *y_offset) == king_piece {
                return true;
            }
        }

        // Rooks and Queens (horizontal and vertical lines)
        let rook_piece = if side_to_move == Color::White {
            Piece::WR
        } else {
            Piece::BR
        };

        let queen_piece = if side_to_move == Color::White {
            Piece::WQ
        } else {
            Piece::BQ
        };

        for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
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
        let bishop_piece = if side_to_move == Color::White {
            Piece::WB
        } else {
            Piece::BB
        };

        for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
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

    pub fn generate_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

        if self.side_to_move == Color::White {
            // White pawns
            for white_pawn in self.piece_list[Piece::WP as usize] {
                // The pawn is not on board
                if white_pawn == Square::None {
                    continue;
                }

                if self.get_piece_with_offset(white_pawn, 0, 1) == Piece::Empty {
                    // Move one square forward
                    if white_pawn.get_rank() == Rank::R7 {
                        // Promotion
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 1), Piece::Empty, false, false, Piece::WQ, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 1), Piece::Empty, false, false, Piece::WR, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 1), Piece::Empty, false, false, Piece::WB, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 1), Piece::Empty, false, false, Piece::WN, CastleType::None, Piece::WP));
                    } else {
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 1), Piece::Empty, false, true, Piece::Empty, CastleType::None, Piece::WP));
                    }

                    // Move two squares forward
                    if white_pawn.get_rank() == Rank::R2
                        && self.get_piece_with_offset(white_pawn, 0, 2) == Piece::Empty
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(0, 2), Piece::Empty, false, true, Piece::Empty, CastleType::None, Piece::WP));
                    }
                }

                // Pawn kill
                if self.get_piece_with_offset(white_pawn, -1, 1) == Piece::BP {
                    #[rustfmt::skip]
                    moves.push(Move::new(white_pawn, white_pawn.new_square(-1, 1), Piece::BP, false, false, Piece::Empty, CastleType::None, Piece::WP));
                } else if self.get_piece_with_offset(white_pawn, 1, 1) == Piece::BP {
                    #[rustfmt::skip]
                    moves.push(Move::new(white_pawn, white_pawn.new_square(1, 1), Piece::BP, false, false, Piece::Empty, CastleType::None, Piece::WP));
                }

                // En passant
                // if pawn to the side is BP and the en passant square is behind that
                if self.en_passant_square != Square::None {
                    if self.get_piece_with_offset(white_pawn, -1, 0) == Piece::BP
                        && white_pawn.new_square(-1, 1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(-1, 1), Piece::BP, false, false, Piece::Empty, CastleType::None, Piece::WP));
                    } else if self.get_piece_with_offset(white_pawn, 1, 0) == Piece::BP
                        && white_pawn.new_square(1, 1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(white_pawn, white_pawn.new_square(1, 1), Piece::BP, false, false, Piece::Empty, CastleType::None, Piece::WP));
                    }
                }
            }

            // White Rook
            for white_rook in self.piece_list[Piece::WR as usize] {
                if white_rook == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_rook.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Rook movement
                            #[rustfmt::skip]
                            moves.push(Move::new(white_rook, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WR));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) != Color::White {
                                break;
                            }
                            // Rook capture
                            #[rustfmt::skip]
                            moves.push(Move::new(white_rook, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WR));
                            break;
                        }
                        step += 1;
                    }
                }

                // Rook castle queen side
                if white_rook == Square::A1
                    && (self.castle_permission & Castling::WQ as u32 != 0)
                    && self.is_square_empty(Square::B1)
                    && self.is_square_empty(Square::C1)
                    && self.is_square_empty(Square::D1)
                    && !self.is_square_attacked(Square::E1, Color::Black)
                    && !self.is_square_attacked(Square::C1, Color::Black)
                    && !self.is_square_attacked(Square::D1, Color::Black)
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::A1, Square::D1, Piece::Empty, false, false, Piece::Empty, CastleType::QueenSide, Piece::WR));
                }

                // Rook castle king side
                if white_rook == Square::H1
                    && (self.castle_permission & Castling::WK as u32 != 0)
                    && self.is_square_empty(Square::F1)
                    && self.is_square_empty(Square::G1)
                    && !self.is_square_attacked(Square::E1, Color::Black)
                    && !self.is_square_attacked(Square::F1, Color::Black)
                    && !self.is_square_attacked(Square::G1, Color::Black)
                    && !self.is_square_attacked(Square::H1, Color::Black)
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::H1, Square::F1, Piece::Empty, false, false, Piece::Empty, CastleType::KingSide, Piece::WR));
                }
            }

            // White Bishop
            for white_bishop in self.piece_list[Piece::WB as usize] {
                if white_bishop == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_bishop.new_coor(x_offset * step, y_offset * step)
                    {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Bishop movement
                            #[rustfmt::skip]
                            moves.push(Move::new(white_bishop, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WB));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Bishop capture
                            #[rustfmt::skip]
                            moves.push(Move::new(white_bishop, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WB));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // White Knight
            for white_knight in self.piece_list[Piece::WN as usize] {
                if white_knight == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in KNIGHT_OFFSETS.iter() {
                    if let Some(index) = white_knight.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            #[rustfmt::skip]
                            moves.push(Move::new(white_knight, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WN));
                        } else if piece_color(&piece) != Color::White {
                            #[rustfmt::skip]
                            moves.push(Move::new(white_knight, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WN));
                        }
                    }
                }
            }

            // White Queen
            for white_queen in self.piece_list[Piece::WQ as usize] {
                if white_queen == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Bishop-type movement
                            #[rustfmt::skip]
                            moves.push(Move::new(white_queen, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WQ));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Bishop-type capture
                            #[rustfmt::skip]
                            moves.push(Move::new(white_queen, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WQ));
                            break;
                        }
                        step += 1;
                    }
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Rook-type movement
                            #[rustfmt::skip]
                            moves.push(Move::new(white_queen, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WQ));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Rook-type capture
                            #[rustfmt::skip]
                            moves.push(Move::new(white_queen, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WQ));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // White King
            for white_king in self.piece_list[Piece::WK as usize] {
                if white_king == Square::None {
                    break;
                }

                for (x_offset, y_offset) in KING_OFFSETS.iter() {
                    if let Some(index) = white_king.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            #[rustfmt::skip]
                            moves.push(Move::new(white_king, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::WK));
                        } else if piece_color(&piece) != Color::White {
                            #[rustfmt::skip]
                            moves.push(Move::new(white_king, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::WK));
                        }
                    }
                }
            }
        } else if self.side_to_move == Color::Black {
            // Black pawns
            for black_pawn in self.piece_list[Piece::BP as usize] {
                if black_pawn == Square::None {
                    continue;
                }

                if self.get_piece_with_offset(black_pawn, 0, -1) == Piece::Empty {
                    // Move one square forward
                    if black_pawn.get_rank() == Rank::R2 {
                        // Promotion
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -1), Piece::Empty, false, false, Piece::BQ, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -1), Piece::Empty, false, false, Piece::BR, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -1), Piece::Empty, false, false, Piece::BB, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -1), Piece::Empty, false, false, Piece::BN, CastleType::None, Piece::BP));
                    } else {
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -1), Piece::Empty, false, true, Piece::Empty, CastleType::None, Piece::BP));
                    }

                    // Move two squares forward
                    if black_pawn.get_rank() == Rank::R7
                        && self.get_piece_with_offset(black_pawn, 0, -2) == Piece::Empty
                    {
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(0, -2), Piece::Empty, false, true, Piece::Empty, CastleType::None, Piece::BP));
                    }
                }

                // Pawn kill
                if self.get_piece_with_offset(black_pawn, -1, -1) == Piece::WP {
                    #[rustfmt::skip]
                        moves.push(Move::new(black_pawn, black_pawn.new_square(-1, -1), Piece::WP, false, false, Piece::Empty, CastleType::None, Piece::BP));
                } else if self.get_piece_with_offset(black_pawn, 1, -1) == Piece::WP {
                    #[rustfmt::skip]
                        moves.push(Move::new(black_pawn, black_pawn.new_square(1, -1), Piece::WP, false, false, Piece::Empty, CastleType::None, Piece::BP));
                }

                // En passant
                if self.en_passant_square != Square::None {
                    if self.get_piece_with_offset(black_pawn, -1, 0) == Piece::WP
                        && black_pawn.new_square(-1, -1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(-1, -1), Piece::WP, false, false, Piece::Empty, CastleType::None, Piece::BP));
                    } else if self.get_piece_with_offset(black_pawn, 1, 0) == Piece::WP
                        && black_pawn.new_square(1, -1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                            moves.push(Move::new(black_pawn, black_pawn.new_square(1, -1), Piece::WP, false, false, Piece::Empty, CastleType::None, Piece::BP));
                    }
                }
            }

            // Black Rook
            for black_rook in self.piece_list[Piece::BR as usize] {
                if black_rook == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_rook.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Rook movement
                            #[rustfmt::skip]
                                moves.push(Move::new(black_rook, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BR));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) != Color::Black {
                                break;
                            }
                            // Rook capture
                            #[rustfmt::skip]
                                moves.push(Move::new(black_rook, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BR));
                            break;
                        }
                        step += 1;
                    }
                }

                // Rook castle queen side
                if black_rook == Square::A8
                    && (self.castle_permission & Castling::BQ as u32 != 0)
                    && self.is_square_empty(Square::B8)
                    && self.is_square_empty(Square::C8)
                    && self.is_square_empty(Square::D8)
                    && !self.is_square_attacked(Square::E8, Color::White)
                    && !self.is_square_attacked(Square::C8, Color::White)
                    && !self.is_square_attacked(Square::D8, Color::White)
                {
                    #[rustfmt::skip]
                        moves.push(Move::new(Square::A8, Square::D8, Piece::Empty, false, false, Piece::Empty, CastleType::QueenSide, Piece::BR));
                }

                // Rook castle king side
                if black_rook == Square::H8
                    && (self.castle_permission & Castling::BK as u32 != 0)
                    && self.is_square_empty(Square::F8)
                    && self.is_square_empty(Square::G8)
                    && !self.is_square_attacked(Square::E8, Color::White)
                    && !self.is_square_attacked(Square::F8, Color::White)
                    && !self.is_square_attacked(Square::G8, Color::White)
                    && !self.is_square_attacked(Square::H8, Color::White)
                {
                    #[rustfmt::skip]
                        moves.push(Move::new(Square::H8, Square::F8, Piece::Empty, false, false, Piece::Empty, CastleType::KingSide, Piece::BR));
                }
            }

            // Black Bishop
            for black_bishop in self.piece_list[Piece::BB as usize] {
                if black_bishop == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_bishop.new_coor(x_offset * step, y_offset * step)
                    {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Bishop movement
                            #[rustfmt::skip]
                                moves.push(Move::new(black_bishop, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BB));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            // Bishop capture
                            #[rustfmt::skip]
                                moves.push(Move::new(black_bishop, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BB));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // Black Knight
            for black_knight in self.piece_list[Piece::BN as usize] {
                if black_knight == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in KNIGHT_OFFSETS.iter() {
                    if let Some(index) = black_knight.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            #[rustfmt::skip]
                                moves.push(Move::new(black_knight, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BN));
                        } else if piece_color(&piece) != Color::Black {
                            #[rustfmt::skip]
                                moves.push(Move::new(black_knight, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BN));
                        }
                    }
                }
            }

            // Black Queen
            for black_queen in self.piece_list[Piece::BQ as usize] {
                if black_queen == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            // Bishop-type movement
                            #[rustfmt::skip]
                                moves.push(Move::new(black_queen, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BQ));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            // Capture
                            #[rustfmt::skip]
                                moves.push(Move::new(black_queen, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BQ));
                            break;
                        }
                        step += 1;
                    }
                }

                // Rook-type movement for queen
                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            #[rustfmt::skip]
                                moves.push(Move::new(black_queen, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BQ));
                        } else if piece != Piece::Empty {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            #[rustfmt::skip]
                                moves.push(Move::new(black_queen, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BQ));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // Black King
            for black_king in self.piece_list[Piece::BK as usize] {
                if black_king == Square::None {
                    continue;
                }

                for (x_offset, y_offset) in KING_OFFSETS.iter() {
                    if let Some(index) = black_king.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::Empty {
                            #[rustfmt::skip]
                                moves.push(Move::new(black_king, index.into(), Piece::Empty, false, false, Piece::Empty, CastleType::None, Piece::BK));
                        } else if piece_color(&piece) != Color::Black {
                            #[rustfmt::skip]
                                moves.push(Move::new(black_king, index.into(), piece, false, false, Piece::Empty, CastleType::None, Piece::BK));
                        }
                    }
                }
            }
        }

        moves
    }

    pub fn is_square_empty(&self, square: Square) -> bool {
        self.get_piece_at_index(square as usize) == Piece::Empty
    }
}
