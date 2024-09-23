use crate::definitions::color::Color;
use crate::definitions::piece::Piece;

#[rustfmt::skip]
pub fn is_piece_big(piece: &Piece) -> bool {[false, false, true, true, true, true, true, false, true, true, true, true, true,][*piece as usize]}
#[rustfmt::skip]
pub fn is_piece_major(piece: &Piece) -> bool {[false, false, false, false, true, true, true, false, false, false, true, true, true,][*piece as usize]}
#[rustfmt::skip]
pub fn is_piece_minor(piece: &Piece) -> bool {[false, false, true, true, false, false, false, false, true, true, false, false, false,][*piece as usize]}
#[rustfmt::skip]
pub fn piece_value(piece: &Piece) -> u32 {[0, 100, 320, 330, 500, 900, 20000, 100, 320, 330, 500, 900, 20000,][*piece as usize]}

//  0, WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK,
#[rustfmt::skip]
pub fn is_piece_knight(piece: &Piece) -> bool {[false, false, true, false, false, false, false, false, true, false, false, false, false,][*piece as usize]}
#[rustfmt::skip]
pub fn is_piece_king(piece: &Piece) -> bool {[false, false, false, false, false, false, true, false, false, false, false, false, true,][*piece as usize]}
#[rustfmt::skip]
pub fn is_piece_rook_or_queen(piece: &Piece) -> bool {[false, false, false, false, true, true, false, false, false, false, true, true, false,][*piece as usize]}
#[rustfmt::skip]
pub fn is_piece_bishop_or_queen(piece: &Piece) -> bool {[false, false, false, true, false, true, false, false, false, true, false, true, false,][*piece as usize]}
#[rustfmt::skip]
pub fn piece_color(piece: &Piece) -> Color {[Color::Both, Color::White, Color::White, Color::White, Color::White, Color::White, Color::White, Color::Black, Color::Black, Color::Black, Color::Black, Color::Black, Color::Black,][*piece as usize]}
