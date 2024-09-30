use chess::*;
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::time::{Duration, Instant};

const SAFETY_MARGIN: u64 = 1;
const DEFAULT_INCREMENT: u64 = 0;

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const AUTHORS: &'static str = env!("CARGO_PKG_AUTHORS");
pub const NAME: &'static str = env!("CARGO_PKG_NAME");

fn calculate_time_per_move(
    is_white: bool,
    wtime: u64,
    btime: u64,
    movestogo: Option<u32>,
    increment: u64,
    safety_margin: u64,
) -> Duration {
    let remaining_time = if is_white { wtime } else { btime };
    let expected_moves = movestogo.unwrap_or(40); // Default to 40 moves if movestogo is not provided

    // Calculate time per move
    let time_per_move = (remaining_time / expected_moves as u64) + increment - safety_margin;

    // Ensure time per move does not exceed remaining time
    Duration::from_millis(time_per_move.min(remaining_time))
}

fn main() {
    let stdin = io::stdin();
    let mut game = Game::new();
    let mut quit = false;
    let mut transposition_table = HashMap::new();

    for line in stdin.lock().lines() {
        let input = match line {
            Ok(line) => line,
            Err(_) => continue,
        };
        let tokens: Vec<&str> = input.split_whitespace().collect();
        if tokens.is_empty() {
            continue;
        }

        match tokens[0] {
            "uci" => {
                println!("id name {} {}", NAME, VERSION);
                println!("id author {}", AUTHORS);
                println!("uciok");
            }
            "isready" => {
                println!("readyok");
            }
            "position" => {
                if tokens[1] == "startpos" {
                    game = Game::new();
                } else if tokens[1] == "fen" {
                    let fen = tokens[2..].join(" ");
                    game = Game::from_str(&fen).expect("Valid position");
                }

                if let Some(moves_index) = tokens.iter().position(|&x| x == "moves") {
                    for mv in &tokens[moves_index + 1..] {
                        let chess_move = uci_to_move(mv).expect("Valid move");
                        game.make_move(chess_move);
                    }
                }
            }
            "go" => {
                let mut time_limit = Duration::from_millis(50); // Default time limit
                let mut wtime = None;
                let mut btime = None;
                let mut movestogo = None;
                let mut increment = DEFAULT_INCREMENT;

                // Parse time parameters from the "go" command
                for i in 0..tokens.len() {
                    match tokens[i] {
                        "wtime" => {
                            wtime = tokens.get(i + 1).and_then(|&t| t.parse::<u64>().ok());
                        }
                        "btime" => {
                            btime = tokens.get(i + 1).and_then(|&t| t.parse::<u64>().ok());
                        }
                        "movestogo" => {
                            movestogo = tokens.get(i + 1).and_then(|&t| t.parse::<u32>().ok());
                        }
                        "movetime" => {
                            if let Some(ms) = tokens.get(i + 1).and_then(|&t| t.parse::<u64>().ok())
                            {
                                time_limit = Duration::from_millis(ms);
                            }
                        }
                        "inc" => {
                            increment = tokens
                                .get(i + 1)
                                .and_then(|&t| t.parse::<u64>().ok())
                                .unwrap_or(DEFAULT_INCREMENT);
                        }
                        _ => {}
                    }
                }

                // Calculate the time limit based on remaining time and moves to go
                if let (Some(wtime), Some(btime)) = (wtime, btime) {
                    let is_white = game.side_to_move() == Color::White;
                    time_limit = calculate_time_per_move(
                        is_white,
                        wtime,
                        btime,
                        movestogo,
                        increment,
                        SAFETY_MARGIN,
                    );
                }

                // Find and make the best move
                if let Some(best_move) =
                    find_best_move(&mut game, time_limit, &mut transposition_table)
                {
                    game.make_move(best_move);
                    println!("bestmove {}", best_move);
                }
            }
            "quit" => {
                quit = true;
            }
            _ => {}
        }

        io::stdout().flush().unwrap();
        if quit {
            break;
        }
    }
}

fn uci_to_move(uci: &str) -> Result<ChessMove, String> {
    let from = Square::from_str(&uci[0..2]).map_err(|_| "Invalid from square")?;
    let to = Square::from_str(&uci[2..4]).map_err(|_| "Invalid to square")?;

    let promotion = if uci.len() == 5 {
        Some(match uci.chars().nth(4).unwrap() {
            'q' => Piece::Queen,
            'r' => Piece::Rook,
            'b' => Piece::Bishop,
            'n' => Piece::Knight,
            _ => return Err("Invalid promotion piece".to_string()),
        })
    } else {
        None
    };

    Ok(ChessMove::new(from, to, promotion))
}

const MAX_DEPTH: usize = 64;

fn find_best_move(
    game: &mut Game,
    time_limit: Duration,
    mut transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
) -> Option<ChessMove> {
    let mut best_move = None;

    let board = game.current_position();
    let start_time = Instant::now();
    let mut rng = thread_rng();

    let mut nodes_searched = 0;

    // History heuristic table: [Piece][Destination] -> Score
    let mut history_heuristic = vec![vec![0; 64]; 12];

    // Killer moves: Track two moves for each depth
    let mut killer_moves = vec![vec![None; 2]; MAX_DEPTH];

    // Principal Variation (PV) tracking
    let pv_table: Vec<Option<ChessMove>> = vec![None; MAX_DEPTH];

    for depth in 1.. {
        if start_time.elapsed() >= time_limit {
            return best_move;
        }

        let legal_moves: Vec<_> = MoveGen::new_legal(&board).collect();
        let mut move_scores: Vec<(ChessMove, i32)> = Vec::new();

        legal_moves.iter().for_each(|&mv| {
            let new_board = board.make_move_new(mv);

            let score = -negamax(
                &new_board,
                depth - 1,
                -20000,
                20000,
                &mut transposition_table,
                &mut nodes_searched,
                &start_time,
                &time_limit,
                1,
            );

            // Apply killer moves boost
            if killer_moves[depth].contains(&Some(mv)) {
                let boosted_score = score + 50;
                move_scores.push((mv, boosted_score));
            } else {
                move_scores.push((mv, score));
            }
        });

        // Sort moves by score and apply Principal Variation Search
        move_scores.sort_by_key(|&(_, score)| -score); // Sort in descending order

        let epsilon = 50;
        let current_best_score = move_scores
            .iter()
            .max_by_key(|(_, score)| *score)
            .unwrap()
            .1;

        let weighted_moves: Vec<_> = move_scores
            .iter()
            .filter(|(_, score)| (*score - current_best_score).abs() <= epsilon)
            .map(|&(mv, score)| {
                let weight = (score - current_best_score).max(0) as f64 + 1.0;
                (mv, weight)
            })
            .collect();

        // Use weighted random selection for slightly worse moves
        if let Some(&(selected_move, _)) =
            weighted_moves.choose_weighted(&mut rng, |item| item.1).ok()
        {
            best_move = Some(selected_move);
        }

        // Update history heuristic for this move
        if let Some(best_mv) = best_move {
            let piece = board.piece_on(best_mv.get_source()).unwrap();
            let dest_square = best_mv.get_dest().to_index();
            history_heuristic[piece as usize][dest_square] += depth as i32;
        }

        // Update killer moves
        if let Some(best_mv) = best_move {
            if let Some(killer1) = killer_moves[depth][0] {
                if best_mv != killer1 {
                    killer_moves[depth][1] = Some(best_mv); // Store as the second killer move
                }
            } else {
                killer_moves[depth][0] = Some(best_mv); // Store as the first killer move
            }
        }

        // Print the Principal Variation
        let mut pv_moves = vec![];
        for d in 1..=depth {
            if let Some(mv) = pv_table[d] {
                pv_moves.push(mv);
            } else {
                break;
            }
        }

        let pv_str: String = pv_moves
            .iter()
            .map(|mv| format!("{}", mv))
            .collect::<Vec<String>>()
            .join(" ");

        let total_elapsed_time = start_time.elapsed().as_micros();
        let nps = if total_elapsed_time != 0 {
            (nodes_searched as u128 * 1_000_000) / total_elapsed_time
        } else {
            1
        };

        println!(
            "info depth {} multipv 1 score cp {} nodes {} nps {} time {} pv {}",
            depth,
            current_best_score,
            nodes_searched,
            nps,
            total_elapsed_time / 1_000,
            pv_str,
        );
    }

    best_move
}

fn quiescence_search(
    board: &Board,
    mut alpha: i32,
    beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    nodes_searched: &mut u64,
    start_time: &Instant,
    time_limit: &Duration,
    color: i32,
    depth: usize,
) -> i32 {
    let stand_pat = evaluate(board, depth);
    if start_time.elapsed() >= *time_limit {
        return stand_pat;
    }

    // Evaluate the static position (stand pat)
    *nodes_searched += 1;

    if stand_pat >= beta {
        return beta;
    }

    if alpha < stand_pat {
        alpha = stand_pat;
    }

    for mv in MoveGen::new_legal(board)
        .filter(|mv| board.piece_on(mv.get_dest()).is_some() || board.checkers().popcnt() > 0)
    {
        if board.piece_on(mv.get_dest()).is_some() {
            let new_board = board.make_move_new(mv);
            let score = -quiescence_search(
                &new_board,
                -beta,
                -alpha,
                transposition_table,
                nodes_searched,
                start_time,
                time_limit,
                -color,
                depth + 1,
            );

            if score >= beta {
                return beta;
            }

            if score > alpha {
                alpha = score;
            }
        }
    }

    alpha
}

fn evaluate_move(board: &Board, mv: ChessMove) -> i32 {
    let mut score = 0;

    if let Some(captured_piece) = board.piece_on(mv.get_dest()) {
        score += match captured_piece {
            Piece::Pawn => PAWN_VALUE,
            Piece::Knight => KNIGHT_VALUE,
            Piece::Bishop => BISHOP_VALUE,
            Piece::Rook => ROOK_VALUE,
            Piece::Queen => QUEEN_VALUE,
            _ => 0,
        };
    }

    if let Some(promotion_piece) = mv.get_promotion() {
        score += match promotion_piece {
            Piece::Queen => QUEEN_VALUE,
            _ => 0,
        };
    }

    let next_board = board.make_move_new(mv);
    if next_board.checkers().popcnt() > 0 {
        score += 10000;
    }

    if let Some(moving_piece) = board.piece_on(mv.get_source()) {
        score += positional_value(moving_piece, mv.get_dest());
    }

    if next_board.status() == BoardStatus::Checkmate {
        score += 20000;
    }

    score
}

fn negamax(
    board: &Board,
    depth: usize,
    mut alpha: i32,
    mut beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    nodes_searched: &mut u64,
    start_time: &Instant,
    time_limit: &Duration,
    color: i32, // 1 for white, -1 for black
) -> i32 {
    if start_time.elapsed() >= *time_limit {
        return 0;
    }

    *nodes_searched += 1;

    let alpha_original = alpha;

    // Check the transposition table for existing entry
    if let Some(entry) = transposition_table.get(board) {
        if entry.depth >= depth {
            match entry.bound {
                Bound::Exact => return entry.score,
                Bound::LowerBound => {
                    alpha = alpha.max(entry.score);
                }
                Bound::UpperBound => {
                    beta = beta.min(entry.score);
                }
            }

            if alpha >= beta {
                return entry.score;
            }
        }
    }

    if board.status() == BoardStatus::Checkmate || board.status() == BoardStatus::Stalemate {
        return evaluate(board, depth) * color;
    }

    // If depth is 0, perform quiescence search
    if depth == 0 {
        return quiescence_search(
            board,
            alpha,
            beta,
            transposition_table,
            nodes_searched,
            start_time,
            time_limit,
            color,
            0,
        );
    }

    // Generate and sort legal moves using the evaluate_move function
    let mut legal_moves: Vec<_> = MoveGen::new_legal(board).collect();
    legal_moves.sort_by_key(|mv| evaluate_move(board, *mv));

    assert!(!legal_moves.is_empty());

    let mut score = -20000;

    // Iterate over legal moves using alpha-beta pruning
    for mv in legal_moves {
        let new_board = board.make_move_new(mv);
        let move_score = -negamax(
            &new_board,
            depth - 1,
            -beta,
            -alpha,
            transposition_table,
            nodes_searched,
            start_time,
            time_limit,
            -color,
        );

        score = score.max(move_score);

        alpha = alpha.max(score);

        if alpha >= beta {
            break;
        }
    }

    // Store the best score as either an exact value or a lower bound
    transposition_table.insert(
        *board,
        TranspositionTableEntry {
            score,
            depth,
            bound: if score <= alpha_original {
                Bound::UpperBound
            } else if score >= beta {
                Bound::LowerBound
            } else {
                Bound::Exact
            },
        },
    );

    score
}

enum Bound {
    Exact,
    LowerBound,
    UpperBound,
}

struct TranspositionTableEntry {
    score: i32,
    depth: usize,
    bound: Bound, // Stores if it's a bound or exact score
}

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

fn evaluate(board: &Board, depth: usize) -> i32 {
    let is_white_turn = board.side_to_move() == Color::White;
    // Checkmate condition
    if board.status() == BoardStatus::Checkmate {
        return if is_white_turn {
            -20000 + depth as i32
        } else {
            20000 - depth as i32
        };
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
            let mobility = MoveGen::new_legal(&board)
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
            if piece == Piece::Rook {
                if rook_on_open_file(sq, board) {
                    if board.color_on(sq) == Some(Color::White) {
                        white_rook_bonus += 50;
                    } else {
                        black_rook_bonus += 50;
                    }
                }
            }
        }
    }

    // Apply a penalty/reward for being in check
    if board.checkers().popcnt() > 0 {
        score += if is_white_turn { -10000 } else { 10000 }; // Penalty for being in check
    }

    // Combine king safety, pawn structure, mobility, and other factors into the final score
    score += white_king_safety - black_king_safety;
    score += white_pawn_structure - black_pawn_structure;
    score += white_mobility - black_mobility;
    score += white_center_control - black_center_control;
    score += white_rook_bonus - black_rook_bonus;
    score += white_bishop_pair_bonus - black_bishop_pair_bonus;

    score
}

// Corrected king_safety_score, pawn_structure_score, and rook_on_open_file functions
// Example function for calculating king safety
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
            Rank::from_index(king_rank.to_index() - 1),
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

// Example function for calculating pawn structure score
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

// Example function for checking if a rook is on an open file
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
