use chess::*;

const PAWN_VALUE: i32 = 100;
const KNIGHT_VALUE: i32 = 320;
const BISHOP_VALUE: i32 = 330;
const ROOK_VALUE: i32 = 500;
const QUEEN_VALUE: i32 = 900;

#[rustfmt::skip]
const PAWN_TABLE: [i32; 64] = [
    0,  0,  0,  0,  0,  0,  0,  0,
    5, 10, 10, -20, -20, 10, 10,  5,
    5, -5, -10,  0,  0, -10, -5,  5,
    0,  0,  0, 20, 20,  0,  0,  0,
    5,  5, 10, 25, 25, 10,  5,  5,
    10, 10, 20, 30, 30, 20, 10, 10,
    50, 50, 50, 50, 50, 50, 50, 50,
    0, 0, 0, 0, 0, 0, 0, 0
];

#[rustfmt::skip]
const KNIGHT_TABLE: [i32; 64] = [
    -50, -40, -30, -30, -30, -30, -40, -50,
    -40, -20, 0, 0, 0, 0, -20, -40,
    -30, 0, 10, 15, 15, 10, 0, -30,
    -30, 5, 15, 20, 20, 15, 5, -30,
    -30, 0, 15, 20, 20, 15, 0, -30,
    -30, 5, 10, 15, 15, 10, 5, -30,
    -40, -20, 0, 5, 5, 0, -20, -40,
    -50, -40, -30, -30, -30, -30, -40, -50
];

#[rustfmt::skip]
const BISHOP_TABLE: [i32; 64] = [
    -20, -10, -10, -10, -10, -10, -10, -20,
    -10, 5, 0, 0, 0, 0, 5, -10,
    -10, 10, 10, 10, 10, 10, 10, -10,
    -10, 0, 10, 10, 10, 10, 0, -10,
    -10, 5, 5, 10, 10, 5, 5, -10,
    -10, 0, 5, 10, 10, 5, 0, -10,
    -10, 0, 0, 0, 0, 0, 0, -10,
    -20, -10, -10, -10, -10, -10, -10, -20
];

#[rustfmt::skip]
const ROOK_TABLE: [i32; 64] = [
    0, 0, 0, 5, 5, 0, 0, 0,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    -5, 0, 0, 0, 0, 0, 0, -5,
    5, 10, 10, 10, 10, 10, 10, 5,
    0, 0, 0, 0, 0, 0, 0, 0
];

#[rustfmt::skip]
const QUEEN_TABLE: [i32; 64] = [
    -20, -10, -10, -5, -5, -10, -10, -20,
    -10, 0, 0, 0, 0, 0, 0, -10,
    -10, 0, 5, 5, 5, 5, 0, -10,
    -5, 0, 5, 5, 5, 5, 0, -5,
    0, 0, 5, 5, 5, 5, 0, -5,
    -10, 5, 5, 5, 5, 5, 0, -10,
    -10, 0, 5, 0, 0, 0, 0, -10,
    -20, -10, -10, -5, -5, -10, -10, -20
];

#[rustfmt::skip]
const KING_TABLE: [i32; 64] = [
    20, 30, 10, 0, 0, 10, 30, 20,
    20, 20, 0, 0, 0, 0, 20, 20,
    -10, -20, -20, -20, -20, -20, -20, -10,
    20, -30, -30, -40, -40, -30, -30, -20,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30,
    -30, -40, -40, -50, -50, -40, -40, -30
];

pub fn evaluate_board(board: &Board) -> i64 {
    let is_white_turn = board.side_to_move() == Color::White;
    // Checkmate condition
    if board.status() == BoardStatus::Checkmate {
        return if is_white_turn { -20000 } else { 20000 };
    } else if board.status() == BoardStatus::Stalemate {
        return 0;
    }

    let mut score = 0;
    let mut white_king_safety = 0;
    let mut black_king_safety = 0;
    let mut white_pawn_structure = 0;
    let mut black_pawn_structure = 0;
    let mut white_mobility = 0;
    let mut black_mobility = 0;
    let mut white_center_control = 0;
    let mut black_center_control = 0;
    let mut white_rook_bonus = 0;
    let mut black_rook_bonus = 0;
    let mut white_bishop_pair_bonus = 0;
    let mut black_bishop_pair_bonus = 0;

    // Squares representing the center
    let center_squares = [Square::E4, Square::D4, Square::E5, Square::D5];

    let black_bishops = (board.pieces(Piece::Bishop) & board.color_combined(Color::Black)).popcnt();
    let white_bishops = (board.pieces(Piece::Bishop) & board.color_combined(Color::White)).popcnt();

    if white_bishops >= 2 {
        white_bishop_pair_bonus = 50; // Reward for bishop pair
    }

    if black_bishops >= 2 {
        black_bishop_pair_bonus = 50; // Reward for bishop pair
    }

    // Iterate over all squares to calculate piece value and positional score
    for sq in 0..64 {
        let sq: Square = unsafe { Square::new(sq) };
        if let Some(piece) = board.piece_on(sq) {
            let base_value = match piece {
                Piece::Pawn => PAWN_VALUE,
                Piece::Knight => KNIGHT_VALUE,
                Piece::Bishop => BISHOP_VALUE,
                Piece::Rook => ROOK_VALUE,
                Piece::Queen => QUEEN_VALUE,
                Piece::King => 0, // King has no base value, but we'll consider king safety separately
            };

            // Positional value based on the side (flip for Black)
            let positional_score = if board.color_on(sq) == Some(Color::White) {
                positional_value(piece, sq)
            } else {
                -positional_value(piece, sq)
            };

            let total_value = if board.color_on(sq) == Some(Color::White) {
                base_value + positional_score
            } else {
                -(base_value + positional_score)
            };

            // Accumulate score, positive for white, negative for black
            score += total_value;

            // King safety
            if piece == Piece::King {
                if board.color_on(sq) == Some(Color::White) {
                    white_king_safety += king_safety_score(sq, board);
                } else {
                    black_king_safety += king_safety_score(sq, board);
                }
            }

            // Pawn structure (isolated, doubled, passed)
            if piece == Piece::Pawn {
                if board.color_on(sq) == Some(Color::White) {
                    white_pawn_structure += pawn_structure_score(sq, board);
                } else {
                    black_pawn_structure += pawn_structure_score(sq, board);
                }
            }

            // Piece mobility (count legal moves)
            let mobility = MoveGen::new_legal(board)
                .filter(|m| m.get_source() == sq)
                .count() as i32;
            if board.color_on(sq) == Some(Color::White) {
                white_mobility += mobility;
            } else {
                black_mobility += mobility;
            }

            // Control of key squares (center control)
            if center_squares.contains(&sq) {
                if board.color_on(sq) == Some(Color::White) {
                    white_center_control += 25;
                } else {
                    black_center_control += 25;
                }
            }

            // Rook on open files
            if piece == Piece::Rook && rook_on_open_file(sq, board) {
                if board.color_on(sq) == Some(Color::White) {
                    white_rook_bonus += 50;
                } else {
                    black_rook_bonus += 50;
                }
            }
        }
    }

    // Apply a penalty/reward for being in check
    if board.checkers().popcnt() > 0 {
        score += if is_white_turn { -50 } else { 50 }; // Penalty for being in check
    }

    // Combine king safety, pawn structure, mobility, and other factors into the final score
    score += white_king_safety - black_king_safety;
    score += white_pawn_structure - black_pawn_structure;
    score += white_mobility - black_mobility;
    score += white_center_control - black_center_control;
    score += white_rook_bonus - black_rook_bonus;
    score += white_bishop_pair_bonus - black_bishop_pair_bonus;

    score.into()
}

fn king_safety_score(king_sq: Square, board: &Board) -> i32 {
    let mut score = 0;

    // Penalize the king for being too exposed (based on rank/file)
    let king_rank = king_sq.get_rank();
    let king_file = king_sq.get_file();

    // Simple heuristic: kings are safer near the center and with pawns in front of them
    let king_center_distance =
        (king_rank.to_index() as i32 - 3).abs() + (king_file.to_index() as i32 - 3).abs();
    score -= king_center_distance * 10; // Penalize distance from the center

    // Check if the king has a pawn shield (pawns in front of the king)
    for file_offset in -1..=1 {
        let new_file = (king_file.to_index() as i32 + file_offset).clamp(0, 7) as u8;
        let shield_sq = Square::make_square(
            Rank::from_index(king_rank.to_index()),
            File::from_index(new_file as usize),
        );
        if let Some(piece) = board.piece_on(shield_sq) {
            if piece == Piece::Pawn {
                score += 50; // Reward for having pawn shield
            }
        }
    }

    // Penalize if king is on an open file or near an enemy rook
    let rook_check = MoveGen::new_legal(board)
        .filter(|&mv| {
            board.piece_on(mv.get_dest()) == Some(Piece::Rook)
                && board.color_on(mv.get_dest()) != board.color_on(king_sq)
        })
        .count();
    score -= rook_check as i32 * 100; // Penalize if threatened by rooks

    score
}

fn pawn_structure_score(pawn_sq: Square, board: &Board) -> i32 {
    let mut score = 0;

    let pawn_rank = pawn_sq.get_rank();
    let pawn_file = pawn_sq.get_file();

    // Penalize isolated pawns (no pawns on adjacent files)
    let is_isolated = |file: File| -> bool {
        for rank_index in 0..8 {
            let sq = Square::make_square(Rank::from_index(rank_index), file);
            if board.piece_on(sq) == Some(Piece::Pawn) {
                return false;
            }
        }
        true
    };

    if (pawn_file == File::A || is_isolated(File::from_index(pawn_file.to_index() - 1)))
        && (pawn_file == File::H || is_isolated(File::from_index(pawn_file.to_index() + 1)))
    {
        score -= 50; // Isolated pawn penalty
    }

    // Penalize doubled pawns (more than one pawn on the same file)
    let pawn_on_same_file = MoveGen::new_legal(board)
        .filter(|&mv| {
            mv.get_dest().get_file() == pawn_file
                && board.piece_on(mv.get_dest()) == Some(Piece::Pawn)
        })
        .count();
    if pawn_on_same_file > 1 {
        score -= 30; // Doubled pawn penalty
    }

    // Reward passed pawns (no enemy pawns in front of them)
    let is_passed_pawn = |file: File| -> bool {
        for rank_index in (pawn_rank.to_index() + 1)..8 {
            let sq = Square::make_square(Rank::from_index(rank_index), file);
            if board.piece_on(sq) == Some(Piece::Pawn) {
                return false;
            }
        }
        true
    };

    if is_passed_pawn(pawn_file) {
        score += 100; // Passed pawn reward
    }

    score
}

fn rook_on_open_file(rook_sq: Square, board: &Board) -> bool {
    let rook_file = rook_sq.get_file();

    // An open file means no pawns on the rook's file
    for rank_index in 0..8 {
        let sq = Square::make_square(Rank::from_index(rank_index), rook_file);
        if board.piece_on(sq) == Some(Piece::Pawn) {
            return false; // Not an open file
        }
    }

    true // Open file if no pawns found
}

fn positional_value(piece: Piece, square: Square) -> i32 {
    let index = square.to_index();

    match piece {
        Piece::Pawn => PAWN_TABLE[index],
        Piece::Knight => KNIGHT_TABLE[index],
        Piece::Bishop => BISHOP_TABLE[index],
        Piece::Rook => ROOK_TABLE[index],
        Piece::Queen => QUEEN_TABLE[index],
        Piece::King => KING_TABLE[index],
    }
}
