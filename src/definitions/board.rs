use std::fmt;
use std::hash::{Hash, Hasher};
use std::time::{Duration, Instant};

use crate::definitions::bitboards::BitBoard;
use crate::definitions::castling::{CastleType, Castling};
use crate::definitions::color::Color;
use crate::definitions::move_::Move;
use crate::definitions::piece::Piece;
use crate::definitions::rank::Rank;
use crate::definitions::square::Square;
use crate::definitions::undo::Undo;
use crate::utils::identification::*;
use crate::utils::offsets::*;
use crate::utils::zobrist::ZOBRIST_TABLE;

use std::collections::HashSet;

use super::piece;

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
    pub piece_list: [HashSet<Square>; 13],
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
                board.fifty_move_counter = parts.get(4)
                    .and_then(|s| s.parse::<u32>().ok()) // Try parsing; return None on failure
                    .unwrap_or(0); // Default to 0 if parsing fails
                board.ply = parts.get(5)
                    .and_then(|s| s.parse::<u32>().ok())
                    .unwrap_or(0) * 2; // Default to 0 if parsing fails
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

impl_type_for_board!(&str);
impl_type_for_board!(String);

impl Default for Board {
    fn default() -> Self {
        let mut pieces = [Piece::None; 64];

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
            piece_list: std::array::from_fn(|_| HashSet::new()), // Initialize empty piece lists
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
                    Piece::None => '.',
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
            pieces: [Piece::None; 64],
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
            piece_list: std::array::from_fn(|_| HashSet::new()),
            big_piece_count: [0; 3],
            major_piece_count: [0; 3],
            minor_piece_count: [0; 3],
            history: Vec::new(),
            material_scores: [0; 3],
        };

        temp.position_key = temp.get_hash();
        temp
    }

    fn get_hash(&self) -> u64 {
        let mut hash = 0;

        for square_index in 0..64 {
            let piece = self.pieces[square_index];
            if piece != Piece::None {
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

    fn update_hash_given_piece(&mut self, piece: Piece, square: Square) {
        self.position_key ^= ZOBRIST_TABLE[piece as usize][square as usize];
    }

    fn update_hash_given_castle_permission(&mut self, castle_permission: u32) {
        self.position_key ^= ZOBRIST_TABLE[13][castle_permission as usize];
    }

    fn update_hash_given_playing_side(&mut self, side: Color) {
        // FIXME: should be side_to_move
        self.position_key ^= ZOBRIST_TABLE[12][0];
    }

    fn update_hash_given_en_passant_square(&mut self, en_passant_square: Square) {
        if en_passant_square != Square::None {
            // FIXME: shouldnt happend
            self.position_key ^= ZOBRIST_TABLE[12][en_passant_square as usize];
        }
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
            if *piece == Piece::None {
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
            self.piece_list[*piece as usize].insert(square);
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
        assert_ne!(square, Square::None);
        self.pieces[square as usize]
    }

    fn get_piece_at_index(&self, index: usize) -> Piece {
        self.get_piece(index.into())
    }

    pub fn set_square(&mut self, square: Square, piece: Piece) {
        assert_ne!(square, Square::None);
        self.pieces[square as usize] = piece;
        self.update_hash_given_piece(piece, square);
        self.material_scores[piece_color(&piece) as usize] += piece_value(&piece);

        if is_piece_big(&piece) {
            self.big_piece_count[piece_color(&piece) as usize] += 1;
        } else if is_piece_major(&piece) {
            self.major_piece_count[piece_color(&piece) as usize] += 1;
        } else if is_piece_minor(&piece) {
            self.minor_piece_count[piece_color(&piece) as usize] += 1;
        } else {
            self.pawn_bitboards[piece_color(&piece) as usize].set(square);
            self.pawn_bitboards[Color::Both as usize].set(square);
        }

        self.piece_list[piece as usize].insert(square);
        self.piece_count[piece as usize] += 1;
        self.piece_count_total += 1;

        if piece == Piece::WK {
            self.king_square[Color::White as usize] = square;
        } else if piece == Piece::BK {
            self.king_square[Color::Black as usize] = square;
        }
    }

    fn set_square_at_index(&mut self, index: usize, piece: Piece) {
        self.set_square(index.into(), piece);
    }

    pub fn clear_square(&mut self, square: Square) {
        let piece = self.get_piece(square);
        if piece == Piece::None {
            return;
        }

        self.pieces[square as usize] = Piece::None;
        self.update_hash_given_piece(piece, square);
        self.material_scores[piece_color(&piece) as usize] -= piece_value(&piece);

        if is_piece_big(&piece) {
            self.big_piece_count[piece_color(&piece) as usize] -= 1;
        } else if is_piece_major(&piece) {
            self.major_piece_count[piece_color(&piece) as usize] -= 1;
        } else if is_piece_minor(&piece) {
            self.minor_piece_count[piece_color(&piece) as usize] -= 1;
        } else {
            self.pawn_bitboards[piece_color(&piece) as usize].clear(square);
            self.pawn_bitboards[Color::Both as usize].clear(square);
        }

        self.piece_list[piece as usize].remove(&square);
        self.piece_count[piece as usize] -= 1;
        self.piece_count_total -= 1;

        if piece == Piece::BK || piece == Piece::WK {
            self.king_square[piece_color(&piece) as usize] = Square::None;
        }
    }

    fn clear_square_at_index(&mut self, index: usize) {
        self.clear_square(index.into());
    }

    pub fn transfer_piece(&mut self, from: Square, to: Square) {
        let piece = self.get_piece(from);
        self.clear_square(from);
        self.set_square(to, piece);
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
    fn get_piece_with_offset(&self, square: &Square, x_offset: isize, y_offset: isize) -> Piece {
        let square_x_corr: isize = *square as isize % 8;
        let square_y_corr: isize = *square as isize / 8;

        if !((0..8).contains(&(square_x_corr + x_offset))
            && (0..8).contains(&(square_y_corr + y_offset)))
        {
            return Piece::None;
        }

        self.pieces[(square_x_corr + x_offset + (square_y_corr + y_offset) * 8) as usize]
    }

    // Is this square attacked by the side to move?
    pub fn is_square_attacked(&self, square: &Square, attacker: &Color) -> bool {
        // Pawns
        if *attacker == Color::White {
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
        let knight_piece = if *attacker == Color::White {
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
        let king_piece = if *attacker == Color::White {
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
        let rook_piece = if *attacker == Color::White {
            Piece::WR
        } else {
            Piece::BR
        };

        let queen_piece = if *attacker == Color::White {
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
                } else if piece != Piece::None {
                    break;
                }
                step += 1;
            }
        }

        // Bishops and Queens (diagonals)
        let bishop_piece = if *attacker == Color::White {
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
                } else if piece != Piece::None {
                    break;
                }
                step += 1;
            }
        }

        false
    }

    pub fn get_pseudo_legal_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();

        if self.side_to_move == Color::White {
            // White King
            for white_king in &self.piece_list[Piece::WK as usize] {
                if *white_king == Square::None {
                    break;
                }

                for (x_offset, y_offset) in KING_OFFSETS.iter() {
                    if self.is_square_attacked(
                        &white_king.new_square(*x_offset, *y_offset),
                        &Color::Black,
                    ) {
                        continue;
                    }

                    if let Some(index) = white_king.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_king, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WK));
                        } else if piece_color(&piece) != Color::White {
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_king, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WK));
                        }
                    }
                }
            }

            // White pawns
            for white_pawn in &self.piece_list[Piece::WP as usize] {
                // The pawn is not on board
                if *white_pawn == Square::None {
                    break;
                }

                if self.get_piece_with_offset(white_pawn, 0, 1) == Piece::None {
                    // Move one square forward
                    if white_pawn.get_rank() == Rank::R7 {
                        // Promotion
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 1), Piece::None, false, false, Piece::WQ, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 1), Piece::None, false, false, Piece::WR, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 1), Piece::None, false, false, Piece::WB, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 1), Piece::None, false, false, Piece::WN, CastleType::None, Piece::WP));
                    } else {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 1), Piece::None, false, true, Piece::None, CastleType::None, Piece::WP));
                    }

                    // Move two squares forward
                    if white_pawn.get_rank() == Rank::R2
                        && self.get_piece_with_offset(white_pawn, 0, 2) == Piece::None
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(0, 2), Piece::None, false, true, Piece::None, CastleType::None, Piece::WP));
                    }
                }

                // Pawn kill
                // CHECK: if this works with enpassant + kill
                let piece_diagonal_left = self.get_piece_with_offset(white_pawn, -1, 1);
                let piece_diagonal_right = self.get_piece_with_offset(white_pawn, 1, 1);
                if white_pawn.get_rank() == Rank::R7 {
                    if piece_diagonal_left != Piece::None
                        && piece_color(&piece_diagonal_left) == Color::Black
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), piece_diagonal_left, false, false, Piece::WQ, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), piece_diagonal_left, false, false, Piece::WR, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), piece_diagonal_left, false, false, Piece::WB, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), piece_diagonal_left, false, false, Piece::WN, CastleType::None, Piece::WP));
                    }

                    if piece_diagonal_right != Piece::None
                        && piece_color(&piece_diagonal_right) == Color::Black
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), piece_diagonal_right, false, false, Piece::WQ, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), piece_diagonal_right, false, false, Piece::WR, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), piece_diagonal_right, false, false, Piece::WB, CastleType::None, Piece::WP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), piece_diagonal_right, false, false, Piece::WN, CastleType::None, Piece::WP));
                    }
                } else {
                    if piece_diagonal_left != Piece::None
                        && piece_color(&piece_diagonal_left) == Color::Black
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), piece_diagonal_left, false, false, Piece::None, CastleType::None, Piece::WP));
                    }

                    if piece_diagonal_right != Piece::None
                        && piece_color(&piece_diagonal_right) == Color::Black
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), piece_diagonal_right, false, false, Piece::None, CastleType::None, Piece::WP));
                    }
                }

                // En passant
                // if pawn to the side is BP and the en passant square is behind that
                if self.en_passant_square != Square::None {
                    if self.get_piece_with_offset(white_pawn, -1, 0) == Piece::BP
                        && white_pawn.new_square(-1, 1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(-1, 1), Piece::BP, true, false, Piece::None, CastleType::None, Piece::WP));
                    } else if self.get_piece_with_offset(white_pawn, 1, 0) == Piece::BP
                        && white_pawn.new_square(1, 1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*white_pawn, white_pawn.new_square(1, 1), Piece::BP, true, false, Piece::None, CastleType::None, Piece::WP));
                    }
                }
            }

            // White Rook
            for white_rook in &self.piece_list[Piece::WR as usize] {
                if *white_rook == Square::None {
                    break;
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_rook.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Rook movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_rook, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WR));
                        } else if piece_color(&piece) == Color::White {
                            break;
                        } else {
                            // Rook capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_rook, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WR));
                            break;
                        }
                        step += 1;
                    }
                }

                // Rook castle queen side
                if *white_rook == Square::A1
                    && (self.castle_permission & Castling::WQ as u32 != 0)
                    && self.is_square_empty(Square::B1)
                    && self.is_square_empty(Square::C1)
                    && self.is_square_empty(Square::D1)
                    && !self.is_square_attacked(&Square::E1, &Color::Black)
                    && !self.is_square_attacked(&Square::C1, &Color::Black)
                    && !self.is_square_attacked(&Square::D1, &Color::Black)
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::E1, Square::C1, Piece::None, false, false, Piece::None, CastleType::QueenSide, Piece::WK));
                }

                // Rook castle king side
                if *white_rook == Square::H1
                    && (self.castle_permission & Castling::WK as u32 != 0)
                    && self.is_square_empty(Square::F1)
                    && self.is_square_empty(Square::G1)
                    && !self.is_square_attacked(&Square::E1, &Color::Black)
                    && !self.is_square_attacked(&Square::F1, &Color::Black)
                    && !self.is_square_attacked(&Square::G1, &Color::Black)
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::E1, Square::G1, Piece::None, false, false, Piece::None, CastleType::KingSide, Piece::WK));
                }
            }

            // White Bishop
            for white_bishop in &self.piece_list[Piece::WB as usize] {
                if *white_bishop == Square::None {
                    break;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_bishop.new_coor(x_offset * step, y_offset * step)
                    {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Bishop movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_bishop, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WB));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Bishop capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_bishop, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WB));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // White Knight
            for white_knight in &self.piece_list[Piece::WN as usize] {
                if *white_knight == Square::None {
                    break;
                }

                for (x_offset, y_offset) in KNIGHT_OFFSETS.iter() {
                    if let Some(index) = white_knight.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_knight, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WN));
                        } else if piece_color(&piece) != Color::White {
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_knight, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WN));
                        }
                    }
                }
            }

            // White Queen
            for white_queen in &self.piece_list[Piece::WQ as usize] {
                if *white_queen == Square::None {
                    break;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Bishop-type movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_queen, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WQ));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Bishop-type capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_queen, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WQ));
                            break;
                        }
                        step += 1;
                    }
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = white_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Rook-type movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_queen, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::WQ));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::White {
                                break;
                            }
                            // Rook-type capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*white_queen, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::WQ));
                            break;
                        }
                        step += 1;
                    }
                }
            }
        } else if self.side_to_move == Color::Black {
            // Black King
            for black_king in &self.piece_list[Piece::BK as usize] {
                if *black_king == Square::None {
                    break;
                }

                for (x_offset, y_offset) in KING_OFFSETS.iter() {
                    if self.is_square_attacked(
                        &black_king.new_square(*x_offset, *y_offset),
                        &Color::White,
                    ) {
                        continue;
                    }

                    if let Some(index) = black_king.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_king, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BK));
                        } else if piece_color(&piece) != Color::Black {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_king, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BK));
                        }
                    }
                }
            }

            // Black pawns
            for black_pawn in &self.piece_list[Piece::BP as usize] {
                if *black_pawn == Square::None {
                    break;
                }

                if self.get_piece_with_offset(&black_pawn, 0, -1) == Piece::None {
                    // Move one square forward
                    if black_pawn.get_rank() == Rank::R2 {
                        // Promotion
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -1), Piece::None, false, false, Piece::BQ, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -1), Piece::None, false, false, Piece::BR, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -1), Piece::None, false, false, Piece::BB, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -1), Piece::None, false, false, Piece::BN, CastleType::None, Piece::BP));
                    } else {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -1), Piece::None, false, true, Piece::None, CastleType::None, Piece::BP));
                    }

                    // Move two squares forward
                    if black_pawn.get_rank() == Rank::R7
                        && self.get_piece_with_offset(&black_pawn, 0, -2) == Piece::None
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(0, -2), Piece::None, false, true, Piece::None, CastleType::None, Piece::BP));
                    }
                }

                // Pawn kill
                let piece_diagonal_left = self.get_piece_with_offset(black_pawn, -1, -1);
                let piece_diagonal_right = self.get_piece_with_offset(black_pawn, 1, -1);
                if black_pawn.get_rank() == Rank::R2 {
                    if piece_diagonal_left != Piece::None
                        && piece_color(&piece_diagonal_left) == Color::White
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), piece_diagonal_left, false, false, Piece::BQ, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), piece_diagonal_left, false, false, Piece::BR, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), piece_diagonal_left, false, false, Piece::BB, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), piece_diagonal_left, false, false, Piece::BN, CastleType::None, Piece::BP));
                    }

                    if piece_diagonal_right != Piece::None
                        && piece_color(&piece_diagonal_right) == Color::White
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), piece_diagonal_right, false, false, Piece::BQ, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), piece_diagonal_right, false, false, Piece::BR, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), piece_diagonal_right, false, false, Piece::BB, CastleType::None, Piece::BP));
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), piece_diagonal_right, false, false, Piece::BN, CastleType::None, Piece::BP));
                    }
                } else {
                    if piece_diagonal_left != Piece::None
                        && piece_color(&piece_diagonal_left) == Color::White
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), piece_diagonal_left, false, false, Piece::None, CastleType::None, Piece::BP));
                    }

                    if piece_diagonal_right != Piece::None
                        && piece_color(&piece_diagonal_right) == Color::White
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), piece_diagonal_right, false, false, Piece::None, CastleType::None, Piece::BP));
                    }
                }

                // En passant
                if self.en_passant_square != Square::None {
                    if self.get_piece_with_offset(&black_pawn, -1, 0) == Piece::WP
                        && black_pawn.new_square(-1, -1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(-1, -1), Piece::WP, true, false, Piece::None, CastleType::None, Piece::BP));
                    } else if self.get_piece_with_offset(&black_pawn, 1, 0) == Piece::WP
                        && black_pawn.new_square(1, -1) == self.en_passant_square
                    {
                        #[rustfmt::skip]
                        moves.push(Move::new(*black_pawn, black_pawn.new_square(1, -1), Piece::WP, true, false, Piece::None, CastleType::None, Piece::BP));
                    }
                }
            }

            // Black Rook
            for black_rook in &self.piece_list[Piece::BR as usize] {
                if *black_rook == Square::None {
                    break;
                }

                for (x_offset, y_offset) in ROOK_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_rook.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Rook movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_rook, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BR));
                        } else if piece_color(&piece) == Color::Black {
                            break;
                        } else {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_rook, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BR));
                            break;
                        }
                        step += 1;
                    }
                }

                // Rook castle queen side
                if *black_rook == Square::A8
                    && (self.castle_permission & Castling::BQ as u32 != 0)
                    && self.is_square_empty(Square::B8)
                    && self.is_square_empty(Square::C8)
                    && self.is_square_empty(Square::D8)
                    && !self.is_square_attacked(&Square::E8, &Color::White)
                    && !self.is_square_attacked(&Square::C8, &Color::White)
                    && !self.is_square_attacked(&Square::D8, &Color::White)
                // Cheking is rook is attacked after castle, FIXME
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::E8, Square::C8, Piece::None, false, false, Piece::None, CastleType::QueenSide, Piece::BK));
                }

                // Rook castle king side
                if *black_rook == Square::H8
                    && (self.castle_permission & Castling::BK as u32 != 0)
                    && self.is_square_empty(Square::F8)
                    && self.is_square_empty(Square::G8)
                    && !self.is_square_attacked(&Square::E8, &Color::White)
                    && !self.is_square_attacked(&Square::F8, &Color::White)
                    && !self.is_square_attacked(&Square::G8, &Color::White)
                {
                    #[rustfmt::skip]
                    moves.push(Move::new(Square::E8, Square::G8, Piece::None, false, false, Piece::None, CastleType::KingSide, Piece::BK));
                }
            }

            // Black Bishop
            for black_bishop in &self.piece_list[Piece::BB as usize] {
                if *black_bishop == Square::None {
                    break;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_bishop.new_coor(x_offset * step, y_offset * step)
                    {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Bishop movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_bishop, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BB));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            // Bishop capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_bishop, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BB));
                            break;
                        }
                        step += 1;
                    }
                }
            }

            // Black Knight
            for black_knight in &self.piece_list[Piece::BN as usize] {
                if *black_knight == Square::None {
                    break;
                }

                for (x_offset, y_offset) in KNIGHT_OFFSETS.iter() {
                    if let Some(index) = black_knight.new_coor(*x_offset, *y_offset) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_knight, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BN));
                        } else if piece_color(&piece) != Color::Black {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_knight, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BN));
                        }
                    }
                }
            }

            // Black Queen
            for black_queen in &self.piece_list[Piece::BQ as usize] {
                if *black_queen == Square::None {
                    break;
                }

                for (x_offset, y_offset) in BISHOP_OFFSETS.iter() {
                    let mut step = 1;
                    while let Some(index) = black_queen.new_coor(x_offset * step, y_offset * step) {
                        let piece = self.get_piece_at_index(index);
                        if piece == Piece::None {
                            // Bishop-type movement
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_queen, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BQ));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            // Capture
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_queen, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BQ));
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
                        if piece == Piece::None {
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_queen, index.into(), Piece::None, false, false, Piece::None, CastleType::None, Piece::BQ));
                        } else if piece != Piece::None {
                            if piece_color(&piece) == Color::Black {
                                break;
                            }
                            #[rustfmt::skip]
                            moves.push(Move::new(*black_queen, index.into(), piece, false, false, Piece::None, CastleType::None, Piece::BQ));
                            break;
                        }
                        step += 1;
                    }
                }
            }
        }

        moves
    }

    pub fn get_legal_moves(&mut self) -> Vec<Move> {
        let pseudo_moves = self.get_pseudo_legal_moves();
        let mut legal_moves = Vec::new();

        for mv in pseudo_moves {
            // Make the move
            self.make_move(mv);

            // Check if the king is in check after the move
            let opposite_color = self.side_to_move.flip();
            let king_square = self.king_square[opposite_color as usize];
            if !self.is_square_attacked(&king_square, &self.side_to_move) {
                // If king is not in check, it's a legal move
                legal_moves.push(mv);
            }

            // Undo the move to restore the original board state
            self.undo_move();
        }

        legal_moves
    }

    pub fn make_move(&mut self, move_: Move) {
        let fifty_move_counter_before = self.fifty_move_counter;
        let castle_permission_before = self.castle_permission;
        let mut en_passant_square_before = self.en_passant_square;
        let position_key_before = self.position_key;

        // Update if capture made
        if move_.captured_piece != Piece::None {
            self.clear_square(move_.to);
        }

        // Move piece from -> to, update piece list and the pieces array
        self.transfer_piece(move_.from, move_.to);

        // Update 50 move rule
        if move_.captured_piece != Piece::None
            || move_.current_piece == Piece::WP
            || move_.current_piece == Piece::BP
        {
            self.fifty_move_counter = 0;
        } else {
            self.fifty_move_counter += 1;
        }

        // Promotions
        if move_.promoted_piece != Piece::None {
            self.clear_square(move_.to);
            self.set_square(move_.to, move_.promoted_piece);
        }

        // En passant, delete the en passanted pawn, update hash
        if move_.is_enpassant {
            let en_passant_pawn;

            if self.side_to_move == Color::White {
                en_passant_pawn = move_.to.new_square(0, -1);
            } else {
                en_passant_pawn = move_.to.new_square(0, 1);
            };

            self.clear_square(en_passant_pawn);
            en_passant_square_before = Square::None;
            self.update_hash_given_en_passant_square(en_passant_square_before);
        }

        // Castling, move the king
        match move_.castle {
            CastleType::KingSide => {
                if self.side_to_move == Color::White {
                    self.transfer_piece(Square::H1, Square::F1);
                    self.castle_permission &= !(Castling::WK as u32);
                } else {
                    self.transfer_piece(Square::H8, Square::F8);
                    self.castle_permission &= !(Castling::BK as u32);
                }
            }
            CastleType::QueenSide => {
                if self.side_to_move == Color::White {
                    self.transfer_piece(Square::A1, Square::D1);
                    self.castle_permission &= !(Castling::WQ as u32);
                } else {
                    self.transfer_piece(Square::A8, Square::D8);
                    self.castle_permission &= !(Castling::BQ as u32);
                }
            }
            CastleType::None => {}
        }
        self.update_hash_given_castle_permission(self.castle_permission);

        // Change side
        self.side_to_move = if self.side_to_move == Color::White {
            Color::Black
        } else {
            Color::White
        };
        self.update_hash_given_playing_side(self.side_to_move);

        // Update castle permission
        if move_.current_piece == Piece::WK {
            self.castle_permission &= !(Castling::WK as u32);
            self.castle_permission &= !(Castling::WQ as u32);
        } else if move_.current_piece == Piece::BK {
            self.castle_permission &= !(Castling::BK as u32);
            self.castle_permission &= !(Castling::BQ as u32);
        } else if move_.current_piece == Piece::WR {
            if move_.from == Square::A1 {
                self.castle_permission &= !(Castling::WQ as u32);
            } else if move_.from == Square::H1 {
                self.castle_permission &= !(Castling::WK as u32);
            }
        } else if move_.current_piece == Piece::BR {
            if move_.from == Square::A8 {
                self.castle_permission &= !(Castling::BQ as u32);
            } else if move_.from == Square::H8 {
                self.castle_permission &= !(Castling::BK as u32);
            }
        }
        self.update_hash_given_castle_permission(self.castle_permission);

        // Increase ply, history_ply
        self.ply += 1;
        self.history_ply += 1;

        self.history.push(Undo {
            move_,
            fifty_move_counter: fifty_move_counter_before,
            castle_permission: castle_permission_before,
            en_passant_square: en_passant_square_before,
            position_key: position_key_before,
        });
    }

    pub fn undo_move(&mut self) {
        let undo = self.history.pop().unwrap();
        let move_ = undo.move_;

        // Move piece to original position
        self.transfer_piece(move_.to, move_.from);

        // Update if capture made
        if move_.captured_piece != Piece::None && move_.is_enpassant == false {
            self.set_square(move_.to, move_.captured_piece);
        }

        // Promotions
        if move_.promoted_piece != Piece::None {
            self.clear_square(move_.from);
            self.set_square(move_.from, move_.current_piece); // Restore original piece
        }

        // En passant, restore the en passanted pawn
        if move_.is_enpassant {
            // Reverse the side_to_move first (the one who just moved)
            let en_passant_square = move_.to; // the target square of the en passant move
            let en_passant_pawn_square;

            if self.side_to_move == Color::White {
                // Black captured, so the white pawn was captured
                en_passant_pawn_square = move_.to.new_square(0, 1);
            } else {
                // White captured, so the black pawn was captured
                en_passant_pawn_square = move_.to.new_square(0, -1);
            }
            self.set_square(en_passant_pawn_square, move_.captured_piece);

            // Clear the en passant square
            self.clear_square(en_passant_square);

            // Update the hash with the en passant square removal
            self.update_hash_given_en_passant_square(en_passant_square);
        }

        // Change side
        self.side_to_move = if self.side_to_move == Color::White {
            Color::Black
        } else {
            Color::White
        };
        self.update_hash_given_playing_side(self.side_to_move);

        // Castling, move the king back
        match move_.castle {
            CastleType::KingSide => {
                if self.side_to_move == Color::White {
                    self.transfer_piece(Square::G1, Square::E1);
                } else {
                    self.transfer_piece(Square::G8, Square::E8);
                }
            }
            CastleType::QueenSide => {
                if self.side_to_move == Color::White {
                    self.transfer_piece(Square::C1, Square::E1);
                } else {
                    self.transfer_piece(Square::C8, Square::E8);
                }
            }
            CastleType::None => {}
        }
        self.update_hash_given_castle_permission(self.castle_permission);

        // Update castle permission
        self.castle_permission = undo.castle_permission;

        // Update en passant square
        self.en_passant_square = undo.en_passant_square;

        // Decrease ply, history_ply
        self.ply -= 1;
        self.history_ply -= 1;

        self.fifty_move_counter = undo.fifty_move_counter;
        self.position_key = undo.position_key;
    }

    pub fn is_square_empty(&self, square: Square) -> bool {
        self.get_piece_at_index(square as usize) == Piece::None
    }

    pub fn perft(&mut self, depth: u32) -> u64 {
        if depth == 0 {
            return 1; // Base case: one leaf node reached
        }

        let mut nodes = 0;

        // Generate all legal moves
        let moves = self.get_legal_moves();

        // Iterate over all moves
        for mv in &moves {
            self.make_move(*mv); // Make the move
            let child_nodes = self.perft(depth - 1); // Recurse to calculate the number of nodes for the remaining depth
            nodes += child_nodes; // Accumulate the total nodes for this depth

            // Print divide results for the first depth (depth-1 for initial call)
            // if depth == 1 {
            //     println!("Move {}: {} nodes", mv, child_nodes);
            // }

            self.undo_move(); // Undo the move to backtrack
        }

        nodes
    }

    pub fn perft_test(&mut self, max_depth: u32) -> u64 {
        // let start_time = Instant::now();
        let mut total_nodes = 0;
        for current_depth in 0..=max_depth {
            let nodes = self.perft(current_depth);
            // println!("Depth {}: {} nodes", current_depth, nodes);
            total_nodes = nodes;
        }

        // let elapsed_time = start_time.elapsed();
        // println!(
        //     "Total nodes: {}, total time elapsed: {:.2?}, time per node: {:.2?}, NPS: {:.0} n/s",
        //     total_nodes,
        //     elapsed_time,
        //     Duration::from_secs_f64((elapsed_time.as_secs_f64() / (total_nodes as f64))),
        //     (total_nodes as f64) / (elapsed_time.as_secs_f64())
        // );

        total_nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn test_perft_illegal_en_passant() {
        // avoid illegal en passant capture
        assert_eq!(Board::new("8/5bk1/8/2Pp4/8/1K6/8/8 w - d6 0 1").perft_test(6), 824064);
        assert_eq!(Board::new("8/8/1k6/8/2pP4/8/5BK1/8 b - d3 0 1").perft_test(6), 824064);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_en_passant_capture_checks_opponent() {
        //en passant capture checks opponent
        assert_eq!(Board::new("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1").perft_test(6), 1440467);
        assert_eq!(Board::new("8/5k2/8/2Pp4/2B5/1K6/8/8 w - d6 0 1").perft_test(6), 1440467);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_short_castling_gives_check() {
        // short castling gives check:
        assert_eq!(Board::new("5k2/8/8/8/8/8/8/4K2R w K - 0 1").perft_test(6), 661072);
        assert_eq!(Board::new("4k2r/8/8/8/8/8/8/5K2 b k - 0 1").perft_test(6), 661072);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_long_castling_gives_check() {
        // long castling gives check
        assert_eq!(Board::new("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1").perft_test(6), 803711);
        assert_eq!(Board::new("r3k3/8/8/8/8/8/8/3K4 b q - 0 1").perft_test(6), 803711);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_castling_including_rook_capture() {
        // castling (including losing cr due to rook capture)
        assert_eq!(Board::new("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1").perft_test(4), 1274206);
        assert_eq!(Board::new("r3k2r/7b/8/8/8/8/1B4BQ/R3K2R b KQkq - 0 1").perft_test(4), 1274206);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_castling_promoted() {
        // castling prevented
        assert_eq!(Board::new("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1").perft_test(4), 1720476);
        assert_eq!(Board::new("r3k2r/8/5Q2/8/8/3q4/8/R3K2R w KQkq - 0 1").perft_test(4), 1720476);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_promote_out_of_check() {
        // promote out of check:
        assert_eq!(Board::new("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1").perft_test(6),3821001);
        assert_eq!(Board::new("3K4/8/8/8/8/8/4p3/2k2R2 b - - 0 1").perft_test(6),3821001);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_discovered_check() {
        // discovered check:
        assert_eq!(Board::new("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1").perft_test(5),1004658);
        assert_eq!(Board::new("5K2/8/1Q6/2N5/8/1p2k3/8/8 w - - 0 1").perft_test(5),1004658);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_promote_to_give_check() {
        // promote to give check:
        assert_eq!(Board::new("4k3/1P6/8/8/8/8/K7/8 w - - 0 1").perft_test(6),217342);
        assert_eq!(Board::new("8/k7/8/8/8/8/1p6/4K3 b - - 0 1").perft_test(6),217342);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_underpromote_to_check() {
        // underpromote to check:
        assert_eq!(Board::new("8/P1k5/K7/8/8/8/8/8 w - - 0 1").perft_test(6),92683);
        assert_eq!(Board::new("8/8/8/8/8/k7/p1K5/8 b - - 0 1").perft_test(6),92683);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_self_stalemate() {
        // self stalemate:
        assert_eq!(Board::new("K1k5/8/P7/8/8/8/8/8 w - - 0 1").perft_test(6),2217);
        assert_eq!(Board::new("8/8/8/8/8/p7/8/k1K5 b - - 0 1").perft_test(6),2217);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_stalemate_checkmate() {
        // stalemate/checkmate:
        assert_eq!(Board::new("8/k1P5/8/1K6/8/8/8/8 w - - 0 1").perft_test(7),567584);
        assert_eq!(Board::new("8/8/8/8/1k6/8/K1p5/8 b - - 0 1").perft_test(7),567584);
    }

    #[test]
    #[rustfmt::skip]
    fn test_perft_double_check() {
        // double check:
        assert_eq!(Board::new("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1").perft_test(4),23527);
        assert_eq!(Board::new("8/5k2/8/5N2/5Q2/2K5/8/8 w - - 0 1").perft_test(4),23527);
    }
}
