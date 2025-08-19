use chess::*;

#[rustfmt::skip]
const MG_PIECE_BONUS: [[[i16; 4]; 8]; 5] = [
    // Knight
    [[-175,-92,-74,-73],[-77,-41,-27,-15],[-61,-17,6,12],[-35,8,40,49],[-34,13,44,51],[-9,22,58,53],[-67,-27,4,37],[-201,-83,-56,-26]],
    // Bishop
    [[-53,-5,-8,-23],[-15,8,19,4],[-7,21,-5,17],[-5,11,25,39],[-12,29,22,31],[-16,6,1,11],[-17,-14,5,0],[-48,1,-14,-23]],
    // Rook
    [[-31,-20,-14,-5],[-21,-13,-8,6],[-25,-11,-1,3],[-13,-5,-4,-6],[-27,-15,-4,3],[-22,-2,6,12],[-2,12,16,18],[-17,-19,-1,9]],
    // Queen
    [[3,-5,-5,4],[-3,5,8,12],[-3,6,13,7],[4,5,9,8],[0,14,12,5],[-4,10,6,8],[-5,6,10,8],[-2,-2,1,-2]],
    // King
    [[271,327,271,198],[278,303,234,179],[195,258,169,120],[164,190,138,98],[154,179,105,70],[123,145,81,31],[88,120,65,33],[59,89,45,-1]],
];

#[rustfmt::skip]
const EG_PIECE_BONUS: [[[i16; 4]; 8]; 5] = [
    // Knight
    [[-96,-65,-49,-21],[-67,-54,-18,8],[-40,-27,-8,29],[-35,-2,13,28],[-45,-16,9,39],[-51,-44,-16,17],[-69,-50,-51,12],[-100,-88,-56,-17]],
    // Bishop
    [[-57,-30,-37,-12],[-37,-13,-17,1],[-16,-1,-2,10],[-20,-6,0,17],[-17,-1,-14,15],[-30,6,4,6],[-31,-20,-1,1],[-46,-42,-37,-24]],
    // Rook
    [[-9,-13,-10,-9],[-12,-9,-1,-2],[6,-8,-2,-6],[-6,1,-9,7],[-5,8,7,-6],[6,1,-7,10],[4,5,20,-5],[18,0,19,13]],
    // Queen
    [[-69,-57,-47,-26],[-55,-31,-22,-4],[-39,-18,-9,3],[-23,-3,13,24],[-29,-6,9,21],[-38,-18,-12,1],[-50,-27,-24,-8],[-75,-52,-43,-36]],
    // King
    [[1,45,85,76],[53,100,133,135],[88,130,169,175],[103,156,172,172],[96,166,199,199],[92,172,184,191],[47,121,116,131],[11,59,73,78]],
];

#[rustfmt::skip]
const MG_PAWN_BONUS: [[i16; 8]; 8] = [
    [0,0,0,0,0,0,0,0],[3,3,10,19,16,19,7,-5],[-9,-15,11,15,32,22,5,-22],[-4,-23,6,20,40,17,4,-8],
    [13,0,-13,1,11,-2,-13,5],[5,-12,-7,22,-8,-5,-15,-8],[-7,7,-3,-13,5,-16,10,-8],[0,0,0,0,0,0,0,0],
];

#[rustfmt::skip]
const EG_PAWN_BONUS: [[i16; 8]; 8] = [
    [0,0,0,0,0,0,0,0],[-10,-6,10,0,14,7,-5,-19],[-10,-10,-10,4,4,3,-6,-4],[6,-2,-8,-4,-13,-12,-10,-9],
    [10,5,4,-5,-5,-5,14,9],[28,20,21,28,30,7,6,13],[0,-11,12,21,25,19,4,7],[0,0,0,0,0,0,0,0],
];

#[rustfmt::skip]
pub const IMBALANCE_TABLE_WHITE: [&[i32]; 6] = [
    &[0],
    &[40, 38],
    &[32, 255, -62],
    &[0, 104, 4, 0],
    &[-26, -2, 47, 105, -208],
    &[-189, 24, 117, 133, -134, -6],
];

#[rustfmt::skip]
pub const IMBALANCE_TABLE_BLACK: [&[i32]; 6] = [
    &[0],
    &[36, 0],
    &[9, 63, 0],
    &[59, 65, 42, 0],
    &[46, 39, 24, -24, 0],
    &[97, 100, -42, 137, 268, 0],
];

const MG_PIECE_VALUES: [i16; 6] = [124, 781, 825, 1276, 2538, 0]; // P,N,B,R,Q,K
const EG_PIECE_VALUES: [i16; 6] = [206, 854, 915, 1380, 2682, 0];

pub fn evaluate_board(board: &Board) -> i32 {
    let mut mg_total = 0i32;
    let mut eg_total = 0i32;
    let mut non_pawn_material = 0i32;
    let mut bishop_count = [0, 0]; // W,B

    // SINGLE PASS through all pieces - calculate everything at once
    for square in *board.combined() {
        if let Some(piece) = board.piece_on(square) {
            let color = board.color_on(square).unwrap();
            let sign = if color == Color::White { 1 } else { -1 };
            let rank = square.get_rank().to_index();
            let file = square.get_file().to_index();
            let piece_idx = get_piece_index(&piece);

            // Material values
            let mg_value = MG_PIECE_VALUES[piece_idx] as i32;
            let eg_value = EG_PIECE_VALUES[piece_idx] as i32;
            mg_total += sign * mg_value;
            eg_total += sign * eg_value;

            // PSQ values
            let (mg_psqt, eg_psqt) = match piece {
                Piece::Pawn => {
                    let mg = MG_PAWN_BONUS[7 - rank][file] as i32;
                    let eg = EG_PAWN_BONUS[7 - rank][file] as i32;
                    (mg, eg)
                }
                _ => {
                    let bonus_idx = piece_idx - 1; // Knight=0, Bishop=1, etc.
                    let file_mirrored = file.min(7 - file);
                    let mg = MG_PIECE_BONUS[bonus_idx][7 - rank][file_mirrored] as i32;
                    let eg = EG_PIECE_BONUS[bonus_idx][7 - rank][file_mirrored] as i32;
                    (mg, eg)
                }
            };
            mg_total += sign * mg_psqt;
            eg_total += sign * eg_psqt;

            // Non-pawn material for phase (always positive)
            if piece != Piece::Pawn && piece != Piece::King {
                non_pawn_material += mg_value;
            }

            // Piece Imbalance
            let mut imbalance_total = 0;
            if piece == Piece::Bishop {
                match color {
                    Color::White => bishop_count[0] += 1,
                    Color::Black => bishop_count[1] += 1,
                }
            }

            if piece != Piece::King {
                for inner_square in *board.combined() {
                    if let Some(inner_piece) = board.piece_on(inner_square) {
                        let inner_piece_idx = get_piece_index(&inner_piece);
                        if inner_piece == Piece::King || inner_piece_idx > piece_idx {
                            continue;
                        }

                        let inner_color = board.color_on(inner_square).unwrap();

                        match inner_color {
                            Color::White => {
                                imbalance_total += sign
                                    * IMBALANCE_TABLE_WHITE[piece_idx + 1][inner_piece_idx + 1];
                            }
                            Color::Black => {
                                imbalance_total += sign
                                    * IMBALANCE_TABLE_BLACK[piece_idx + 1][inner_piece_idx + 1];
                            }
                        }
                    }
                }

            if bishop_count[0] > 1 {
                imbalance_total += sign * IMBALANCE_TABLE_WHITE[piece_idx + 1][0]
            }

            if bishop_count[1] > 1 {
                imbalance_total += sign * IMBALANCE_TABLE_BLACK[piece_idx + 1][0]
            }

            }

            imbalance_total += if bishop_count[0] == 2 { 1438 } else { 0 }
                - if bishop_count[1] == 2 { 1438 } else { 0 };
            mg_total += imbalance_total / 16;
            eg_total += imbalance_total / 16;

            // Pawns
        }
    }

    // Apply scale factor to endgame
    eg_total = eg_total * scale_factor(board, eg_total) / 64;

    // Calculate phase
    const MIDGAME_LIMIT: i32 = 15258;
    const ENDGAME_LIMIT: i32 = 3915;
    let phase = {
        let clamped = non_pawn_material.clamp(ENDGAME_LIMIT, MIDGAME_LIMIT);
        ((clamped - ENDGAME_LIMIT) * 128) / (MIDGAME_LIMIT - ENDGAME_LIMIT)
    };

    // Interpolate between middle game and endgame
    let mut value = (mg_total * phase + eg_total * (128 - phase)) / 128;

    // Add tempo
    value += 28
        * if board.side_to_move() == Color::White {
            1
        } else {
            -1
        };

    // Apply rule50 scaling
    let rule50_val = rule50(board);
    value = value * (100 - rule50_val) / 100;

    value
}

// Stub implementations
fn rule50(_board: &Board) -> i32 {
    0
}
fn scale_factor(_board: &Board, _eg_eval: i32) -> i32 {
    64
}

fn get_piece_index(piece: &Piece) -> usize {
    match piece {
        Piece::Pawn => 0,
        Piece::Knight => 1,
        Piece::Bishop => 2,
        Piece::Rook => 3,
        Piece::Queen => 4,
        Piece::King => 5,
    }
}
