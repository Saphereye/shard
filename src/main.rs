use chess::{Board, BoardStatus, ChessMove, Color, Game, MoveGen, Piece, Square};
use std::cmp::max;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::str::FromStr;
use std::time::{Duration, Instant};

const SAFETY_MARGIN: u64 = 50;
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

                    println!("Time limit: {:?}", time_limit);
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
    let mut previous_nodes_searched: u128 = 0;
    let mut nodes_in_previous_layer: u128 = 1;

    let board = game.current_position();
    let start_time = Instant::now();

    for depth in 1..=7 {
        let legal_moves = MoveGen::new_legal(&board);
        let mut pv_line = vec![]; // PV line for the best sequence of moves

        for mv in legal_moves {
            let new_board = board.make_move_new(mv);

            // Search the position recursively, retrieving the score
            let score = search(
                &new_board,
                depth - 1,
                i32::MIN,
                i32::MAX,
                transposition_table,
                &mut pv_line,
                &mut nodes_searched, // Pass the mutable reference
                &start_time,
                &time_limit,
            );

            if score > best_score {
                best_score = score;
                best_move = Some(mv);
                // Update the PV line with the current move
                pv_line.insert(0, mv);
            }
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
            "info depth {} seldepth {} multipv 1 score cp {} nodes {} nps {} hashfull 209 tbhits 0 time {} pv {}",
            depth,
            0, // seldepth can be modified based on your search logic
            best_score,
            nodes_searched,
            nps,
            total_elapsed_time / 1_000, // Convert microseconds back to milliseconds for display
            pv_line
                .iter()
                .map(|mv| format!("{}", mv))
                .collect::<Vec<String>>()
                .join(" ")
        );
    }

    best_move
}

fn quiescence_search(
    board: &Board,
    mut alpha: i32,
    beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    nodes_searched: &mut u128, // Mutable reference to the nodes_searched counter
) -> i32 {
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
    let legal_moves = MoveGen::new_legal(board);

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

fn search(
    board: &Board,
    depth: i32,
    mut alpha: i32,
    beta: i32,
    transposition_table: &mut HashMap<Board, TranspositionTableEntry>,
    pv_line: &mut Vec<ChessMove>, // Mutable reference to the PV line
    nodes_searched: &mut u128,    // Mutable reference to the nodes_searched counter
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
        return quiescence_search(board, alpha, beta, transposition_table, nodes_searched);
    }

    let mut best_score = i32::MIN;
    let mut best_move = None;
    let legal_moves = MoveGen::new_legal(board);

    for mv in legal_moves {
        let new_board = board.make_move_new(mv);
        let score = -search(
            &new_board,
            depth - 1,
            -beta,
            -alpha,
            transposition_table,
            pv_line,
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

fn evaluate(board: &Board) -> i32 {
    let mut score = 0;

    for sq in 0..64 {
        let sq: Square = unsafe { Square::new(sq) };
        if let Some(piece) = board.piece_on(sq) {
            let value = match piece {
                Piece::Pawn => PAWN_VALUE + positional_value(piece, sq),
                Piece::Knight => KNIGHT_VALUE + positional_value(piece, sq),
                Piece::Bishop => BISHOP_VALUE + positional_value(piece, sq),
                Piece::Rook => ROOK_VALUE + positional_value(piece, sq),
                Piece::Queen => QUEEN_VALUE + positional_value(piece, sq),
                Piece::King => KING_VALUE + positional_value(piece, sq),
            };

            if board.status() == BoardStatus::Checkmate {
                if board.side_to_move() == Color::White {
                    score = -20000;
                } else {
                    score = 20000;
                }
            }

            if board.color_on(sq) == Some(Color::White) {
                score += value;
            } else {
                score -= value;
            }
        }
    }

    score
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
