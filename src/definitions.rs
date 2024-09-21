use rand::Rng;
use std::fmt;
use std::sync::LazyLock;

#[rustfmt::skip]
#[derive(PartialEq, Eq, Clone, Copy)]
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

#[derive(PartialEq, Eq)]
pub enum Color {
    White = 0,
    Black,
    None,
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
    A8, B8, C8, D8, E8, F8, G8, H8, NoSquare,
}

macro_rules! impl_type_for_square {
    ($type:ty) => {
        impl From<$type> for Square {
            #[rustfmt::skip]
            fn from(value: $type) -> Self {
                        match value {
                            0 => Square::A1, 1 => Square::B1, 2 => Square::C1, 3 => Square::D1,
                            4 => Square::E1, 5 => Square::F1, 6 => Square::G1, 7 => Square::H1,
                            8 => Square::A2, 9 => Square::B2, 10 => Square::C2, 11 => Square::D2,
                            12 => Square::E2, 13 => Square::F2, 14 => Square::G2, 15 => Square::H2,
                            16 => Square::A3, 17 => Square::B3, 18 => Square::C3, 19 => Square::D3,
                            20 => Square::E3, 21 => Square::F3, 22 => Square::G3, 23 => Square::H3,
                            24 => Square::A4, 25 => Square::B4, 26 => Square::C4, 27 => Square::D4,
                            28 => Square::E4, 29 => Square::F4, 30 => Square::G4, 31 => Square::H4,
                            32 => Square::A5, 33 => Square::B5, 34 => Square::C5, 35 => Square::D5,
                            36 => Square::E5, 37 => Square::F5, 38 => Square::G5, 39 => Square::H5,
                            40 => Square::A6, 41 => Square::B6, 42 => Square::C6, 43 => Square::D6,
                            44 => Square::E6, 45 => Square::F6, 46 => Square::G6, 47 => Square::H6,
                            48 => Square::A7, 49 => Square::B7, 50 => Square::C7, 51 => Square::D7,
                            52 => Square::E7, 53 => Square::F7, 54 => Square::G7, 55 => Square::H7,
                            56 => Square::A8, 57 => Square::B8, 58 => Square::C8, 59 => Square::D8,
                            60 => Square::E8, 61 => Square::F8, 62 => Square::G8, 63 => Square::H8,
                            _ => Square::NoSquare,
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

pub struct Undo {
    move_: u32,
    en_passant_square: Square,
    fifty_move_counter: u32,
    castle_permission: u32,
    position_key: u64,
}

static ZOBRIST_TABLE: LazyLock<[[u64; 64]; 14]> = std::sync::LazyLock::new(|| {
    let mut rng = rand::thread_rng();
    let mut table = [[0u64; 64]; 14];

    for piece in 0..14 {
        for square in 0..64 {
            // Generate a random u64 value for each piece-square combination
            table[piece][square] = rng.gen();
        }
    }

    table
});

pub struct Board {
    /// Array of 64 squares
    pub pieces: [Piece; 64],
    /// Bitboards for each piece type: White, Black, Both
    pub pawn_bitboards: [u64; 3],
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
    /// List of pieces on the board for each piece type,
    /// e.g. to add knights to the board,
    /// we can do pieces\[WN\]\[0\] = E1 then piece_list\[WN\]\[1\] = E4
    pub piece_list: [[u32; 10]; 13],
    /// Number of non-pawn pieces on the board. White, Black, Both
    pub big_piece_count: [u32; 3],
    /// Number of rooks and queens on the board. White, Black, Both
    pub major_piece_count: [u32; 3],
    /// Number of bishops and knights on the board. White, Black, Both
    pub minor_piece_count: [u32; 3],
    /// History of the game
    pub history: Vec<Undo>,
}

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
        for i in 8..16 {
            pieces[i] = Piece::WP; // White Pawns
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
        for i in 48..56 {
            pieces[i] = Piece::BP; // Black Pawns
        }

        Board {
            pieces,
            pawn_bitboards: [0; 3],                // Update later as needed
            king_square: [Square::E1, Square::E8], // Standard starting positions for kings
            side_to_move: Color::White,            // White to move
            en_passant_square: Square::NoSquare,   // No en passant at start
            fifty_move_counter: 0,                 // Initial counter
            ply: 0,                                // Initial ply
            history_ply: 0,                        // Initial history ply
            castle_permission: 0b1111,             // Both sides can castle both ways
            position_key: 0,                       // Initial Zobrist key
            piece_count: [0; 13],                  // Update counts after initializing pieces
            piece_list: [[0; 10]; 13],             // Initialize empty piece lists
            big_piece_count: [0; 3],               // Big piece counts initialized to 0
            major_piece_count: [0; 3],             // Major piece counts initialized to 0
            minor_piece_count: [0; 3],             // Minor piece counts initialized to 0
            history: Vec::new(),                   // Empty history
        }
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
        write!(f, "  a b c d e f g h") // Print the file labels
    }
}

impl Board {
    pub fn new() -> Self {
        Board {
            pieces: [Piece::Empty; 64], // Assuming Piece::Empty is the default piece
            pawn_bitboards: [0; 3],     // Default bitboards initialized to 0
            king_square: [Square::NoSquare; 2], // Assuming Square::NoSquare is the default value
            side_to_move: Color::White, // Default to White
            en_passant_square: Square::NoSquare, // Default value for en passant
            fifty_move_counter: 0,      // Initial counter
            ply: 0,                     // Initial ply
            history_ply: 0,             // Initial history ply
            castle_permission: 0,       // No castling permissions
            position_key: 0,            // Initial Zobrist key
            piece_count: [0; 13],       // Initialize counts for each piece type to 0
            piece_list: [[0; 10]; 13],  // Empty piece lists
            big_piece_count: [0; 3],    // Big piece counts initialized to 0
            major_piece_count: [0; 3],  // Major piece counts initialized to 0
            minor_piece_count: [0; 3],  // Minor piece counts initialized to 0
            history: Vec::new(),        // Empty history
        }
    }

    pub fn get_hash(&self) -> u64 {
        let mut hash = 0;

        for square in 0..64 {
            let piece = self.pieces[square];
            if piece != Piece::Empty {
                hash ^= ZOBRIST_TABLE[piece as usize][square as usize];
            }
        }

        if self.side_to_move == Color::Black {
            hash ^= ZOBRIST_TABLE[12][0];
        }

        if self.en_passant_square != Square::NoSquare {
            hash ^= ZOBRIST_TABLE[12][self.en_passant_square as usize];
        }

        hash ^= ZOBRIST_TABLE[13][self.castle_permission as usize];

        hash
    }
}
