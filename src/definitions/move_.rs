use crate::definitions::castling::CastleType;
use crate::definitions::piece::Piece;
use crate::definitions::square::Square;
use std::fmt::{write, Display, Formatter};

#[derive(Debug, Clone, Copy)]
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
        // Return UCI notation
        let mut notation = String::new();
        notation.push_str(&self.from.to_string());
        notation.push_str(&self.to.to_string());
        if self.promoted_piece != Piece::None {
            notation.push_str(&self.promoted_piece.to_string());
        }
        write!(f, "{}", notation)

        // let mut notation = String::new();

        // match self.castle {
        //     CastleType::KingSide => {
        //         // Handle king-side castling
        //         notation.push_str("O-O");
        //     }
        //     CastleType::QueenSide => {
        //         // Handle queen-side castling
        //         notation.push_str("O-O-O");
        //     }
        //     CastleType::None => {
        //         // Handle normal moves and captures
        //         if self.current_piece != Piece::WP && self.current_piece != Piece::BP {
        //             // For non-pawn pieces, add the piece type
        //             notation.push(format!("{}{}", self.current_piece, self.from.get_file()));
        //         }

        //         // Handle captures
        //         if self.captured_piece != Piece::None {
        //             if self.current_piece == Piece::WP || self.current_piece == Piece::BP {
        //                 // Pawn captures should include the file of the pawn
        //                 notation.push(
        //                     format!("{}", self.from.get_file())
        //                         .to_lowercase()
        //                         .chars()
        //                         .next()
        //                         .unwrap(),
        //                 );
        //             }
        //             notation.push('x');
        //         }

        //         // Add the destination square
        //         notation.push_str(&self.to.to_string());

        //         // Handle promotion
        //         if self.promoted_piece != Piece::None {
        //             notation.push('=');
        //             notation.push(
        //                 format!("{}", self.promoted_piece)
        //                     .to_uppercase()
        //                     .chars()
        //                     .next()
        //                     .unwrap(),
        //             );
        //         }
        //     }
        // }

        // // Write the constructed notation
        // write!(f, "{}", notation)
    }
}
