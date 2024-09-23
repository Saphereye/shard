use crate::definitions::move_::Move;
use crate::definitions::square::Square;

#[derive(Debug)]
pub struct Undo {
    pub move_: Move,
    pub en_passant_square: Square,
    pub fifty_move_counter: u32,
    pub castle_permission: u32,
    pub position_key: u64,
}
