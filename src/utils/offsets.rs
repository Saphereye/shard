#[rustfmt::skip]
pub const KING_OFFSETS: [(isize, isize); 8] = [
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1,  0),
    (-1,  1), (0,  1), (1,  1),
];

#[rustfmt::skip]
pub const KNIGHT_OFFSETS: [(isize, isize); 8] = [
    (-1, -2), (1, -2), (-2, -1), (2, -1),
    (-2,  1), (2,  1), (-1,  2), (1,  2),
];

pub const ROOK_OFFSETS: [(isize, isize); 4] = [(-1, 0), (1, 0), (0, -1), (0, 1)];

pub const BISHOP_OFFSETS: [(isize, isize); 4] = [(-1, -1), (1, -1), (-1, 1), (1, 1)];
