use chess::{Board, BoardStatus, ChessMove, Color, Game, MoveGen, Piece, Square};
use rand::prelude::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::time::{Duration, Instant};

const SAFETY_MARGIN: u64 = 1;
const DEFAULT_INCREMENT: u64 = 0;

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
                println!("id name Shard");
                println!("id author Saphereye");
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

fn find_best_move(
    game: &mut Game,
    time_limit: Duration,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
) -> Option<ChessMove> {
    let mut best_move = None;
    let mut best_score = i32::MIN;
    let mut nodes_searched: u128 = 0;

    let board = game.current_position();
    let start_time = Instant::now();
    let mut rng = thread_rng();

    for depth in 1.. {
        if start_time.elapsed() >= time_limit {
            return best_move;
        }

        let legal_moves: Vec<_> = MoveGen::new_legal(&board).collect();
        let mut move_scores = Vec::new();

        // Calculate the score for each move
        for &mv in &legal_moves {
            let new_board = board.make_move_new(mv);
            let score = search(
                &new_board,
                depth - 1,
                i32::MIN,
                i32::MAX,
                transposition_table,
                &mut nodes_searched,
                &start_time,
                &time_limit,
            );

            move_scores.push((mv, score));
            if score > best_score {
                best_score = score;
                best_move = Some(mv);
            }
        }

        let epsilon = 50; // Allow slight randomness within a margin of score difference
        let best_score = move_scores
            .iter()
            .max_by_key(|(_, score)| *score)
            .unwrap()
            .1;

        let weighted_moves: Vec<_> = move_scores
            .iter()
            .filter(|(_, score)| (*score - best_score).abs() <= epsilon) // Filter moves close to best score
            .map(|&(mv, score)| {
                let weight = (score - best_score).max(0) as f64 + 1.0;
                (mv, weight)
            })
            .collect();

        if let Some(&(selected_move, _)) =
            weighted_moves.choose_weighted(&mut rng, |item| item.1).ok()
        {
            best_move = Some(selected_move);
        }

        // Calculate elapsed time and nodes per second
        let total_elapsed_time = start_time.elapsed().as_micros();
        let nps = if total_elapsed_time != 0 {
            (nodes_searched as u128 * 1_000_000) / total_elapsed_time
        } else {
            1
        };

        // Print the info line with the updated PV
        println!(
            "info depth {} multipv 1 score cp {} nodes {} nps {} time {}",
            depth,
            best_score,
            nodes_searched,
            nps,
            total_elapsed_time / 1_000,
        );
    }

    best_move
}

fn is_check(board: &Board, mv: &ChessMove) -> bool {
    let new_board = board.make_move_new(*mv);
    return new_board.checkers().popcnt() != 0;
}

fn quiescence_search(
    board: &Board,
    mut alpha: i32,
    beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    nodes_searched: &mut u128,
    start_time: &Instant,
    time_limit: &Duration,
) -> i32 {
    if start_time.elapsed() >= *time_limit {
        return 0;
    }

    // Evaluate the static position (stand pat)
    let stand_pat = evaluate(board);
    *nodes_searched += 1;

    // If stand pat evaluation already refutes the position, return beta
    if stand_pat >= beta {
        return beta;
    }

    // Update alpha if stand pat is a better score
    if alpha < stand_pat {
        alpha = stand_pat;
    }

    // Generate all legal moves
    let legal_moves = MoveGen::new_legal(board)
        .filter(|mv| board.piece_on(mv.get_dest()).is_some() || is_check(board, mv)); // Capture or check moves only

    for mv in legal_moves {
        // Check if the move is a capture by checking if the destination square has an opponent's piece
        if board.piece_on(mv.get_dest()).is_some() {
            // Push the move and search deeper
            let new_board = board.make_move_new(mv);
            let score = -quiescence_search(
                &new_board,
                -beta,
                -alpha,
                transposition_table,
                nodes_searched,
                start_time,
                time_limit,
            );

            if score >= beta {
                return beta; // Beta cut-off
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

    // Get destination square and the piece on it (if any)
    if let Some(captured_piece) = board.piece_on(mv.get_dest()) {
        // Reward capturing higher-value pieces
        score += match captured_piece {
            Piece::Pawn => PAWN_VALUE,
            Piece::Knight => KNIGHT_VALUE,
            Piece::Bishop => BISHOP_VALUE,
            Piece::Rook => ROOK_VALUE,
            Piece::Queen => QUEEN_VALUE,
            _ => 0,
        };
    }

    // Reward for promotions, assuming promoting to Queen is the best option
    if let Some(promotion_piece) = mv.get_promotion() {
        score += match promotion_piece {
            Piece::Queen => QUEEN_VALUE,
            Piece::Rook => ROOK_VALUE,
            Piece::Bishop => BISHOP_VALUE,
            Piece::Knight => KNIGHT_VALUE,
            _ => 0,
        };
    }

    // Check if the move results in a check and reward it
    let next_board = board.make_move_new(mv);
    if next_board.checkers().popcnt() > 0 {
        score += 10000; // Arbitrary score for putting the opponent in check
    }

    // Positional value for the destination square
    if let Some(moving_piece) = board.piece_on(mv.get_source()) {
        score += positional_value(moving_piece, mv.get_dest());
    }

    score
}

fn search(
    board: &Board,
    depth: i32,
    mut alpha: i32,
    beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    nodes_searched: &mut u128, // Mutable reference to the nodes_searched counter
    start_time: &Instant,
    time_limit: &Duration,
) -> i32 {
    if start_time.elapsed() >= *time_limit {
        return 0;
    }

    *nodes_searched += 1;

    // Check transposition table for an already evaluated position
    if let Some(entry) = transposition_table.get(board) {
        if entry.depth >= depth {
            return entry.score; // Use stored evaluation if depth is sufficient
        }
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
        );
    }

    let mut best_score = i32::MIN;
    let mut best_move = None;
    let mut legal_moves: Vec<_> = MoveGen::new_legal(board).collect();
    legal_moves.sort_by_key(|mv| evaluate_move(board, *mv)); // Simple heuristic for sorting

    for mv in legal_moves {
        let new_board = board.make_move_new(mv);
        let score = -search(
            &new_board,
            depth - 1,
            -beta,
            -alpha,
            transposition_table,
            nodes_searched,
            start_time,
            time_limit,
        );

        if score > best_score {
            best_score = score;
            best_move = Some(mv);
            alpha = score.max(alpha);
        }

        if alpha >= beta {
            break; // Beta cut-off
        }
    }

    // Store the best move and score in the transposition table
    transposition_table.insert(
        *board,
        TranspositionTableEntry {
            score: best_score,
            depth,
            best_move,
        },
    );

    best_score
}

#[derive(Clone, Copy)]
struct TranspositionTableEntry {
    score: i32,
    depth: i32,
    best_move: Option<ChessMove>, // Store the best move for PV reconstruction
}

const PAWN_VALUE: i32 = 100;
const KNIGHT_VALUE: i32 = 320;
const BISHOP_VALUE: i32 = 330;
const ROOK_VALUE: i32 = 500;
const QUEEN_VALUE: i32 = 900;
const KING_VALUE: i32 = 20000;

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

#[rustfmt::skip]
fn evaluate(board: &Board) -> i32 {
    let mut score = 0;
    let multiplier = if board.side_to_move() == Color::White { 1 } else { -1 };

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
                _ => 0,
            };

            // Add positional value based on piece-square tables
            let positional_score = positional_value(piece, sq);

            let total_value = base_value + positional_score;

            // Accumulate score, positive for white, negative for black
            if board.color_on(sq) == Some(Color::White) {
                score += total_value;
            } else {
                score -= total_value;
            }
        }
    }

    // Apply a penalty or reward for being in check or checkmate
    if board.checkers().popcnt() > 0 {
        score += if board.side_to_move() == Color::White { -10000 } else { 10000 };
    }

    // Final adjustment for checkmate status
    if board.status() == BoardStatus::Checkmate {
        score = if board.side_to_move() == Color::White { -20000 } else { 20000 };
    }

    score * multiplier
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
