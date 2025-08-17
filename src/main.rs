use chess::*;
use std::{
    fmt::Debug,
    str::FromStr,
    time::{Duration, Instant},
};

mod evaluate;
use evaluate::evaluate_board;

mod uci;
use uci::*;

// Optimized transposition table entry - packed into 16 bytes
#[derive(Clone, Debug)]
struct TranspositionEntry {
    key: u32,          // Upper 32 bits of hash for verification
    best_move: u16,    // Packed move representation
    depth: u8,
    score: i16,
    node_type: u8,     // 0=Exact, 1=LowerBound, 2=UpperBound
    age: u8,           // For replacement scheme
}

impl TranspositionEntry {
    fn node_type(&self) -> NodeType {
        match self.node_type {
            0 => NodeType::Exact,
            1 => NodeType::LowerBound,
            _ => NodeType::UpperBound,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum NodeType {
    Exact,
    LowerBound,
    UpperBound,
}

// Killer moves for better move ordering
#[derive(Debug)]
struct KillerMoves {
    moves: [[Option<ChessMove>; 2]; 64], // 2 killer moves per ply
}

impl Default for KillerMoves {
    fn default() -> Self {
        Self {
            moves: [[None; 2]; 64],
        }
    }
}

impl KillerMoves {
    fn add_killer(&mut self, ply: usize, chess_move: ChessMove) {
        if ply >= 64 { return; }
        
        // Don't add the same move twice
        if self.moves[ply][0] == Some(chess_move) {
            return;
        }
        
        // Shift moves
        self.moves[ply][1] = self.moves[ply][0];
        self.moves[ply][0] = Some(chess_move);
    }
    
    fn is_killer(&self, ply: usize, chess_move: ChessMove) -> bool {
        if ply >= 64 { return false; }
        self.moves[ply][0] == Some(chess_move) || self.moves[ply][1] == Some(chess_move)
    }
}

// History heuristic for move ordering
#[derive(Debug)]
struct HistoryTable {
    table: [[[i32; 64]; 64]; 2], // [color][from][to]
}

impl Default for HistoryTable {
    fn default() -> Self {
        Self {
            table: [[[0; 64]; 64]; 2],
        }
    }
}

impl HistoryTable {
    fn add_history(&mut self, color: Color, chess_move: ChessMove, depth: u8) {
        let color_idx = if color == Color::White { 0 } else { 1 };
        let from = chess_move.get_source().to_index();
        let to = chess_move.get_dest().to_index();
        
        let bonus = (depth as i32) * (depth as i32);
        self.table[color_idx][from][to] += bonus;
        
        // Prevent overflow
        if self.table[color_idx][from][to] > 10000 {
            self.age_history();
        }
    }
    
    fn get_history(&self, color: Color, chess_move: ChessMove) -> i32 {
        let color_idx = if color == Color::White { 0 } else { 1 };
        let from = chess_move.get_source().to_index();
        let to = chess_move.get_dest().to_index();
        
        self.table[color_idx][from][to]
    }
    
    fn age_history(&mut self) {
        for color in 0..2 {
            for from in 0..64 {
                for to in 0..64 {
                    self.table[color][from][to] /= 2;
                }
            }
        }
    }
}

#[derive(Debug, Default)]
struct SearchStats {
    nodes_searched: u64,
    transposition_hits: u64,
    beta_cutoffs: u64,
    null_move_cutoffs: u64,
}

pub struct ChessEngine {
    // Use a vector for better cache performance
    transposition_table: Vec<Option<TranspositionEntry>>,
    tt_size: usize,
    tt_age: u8,
    stats: SearchStats,
    start_time: Option<Instant>,
    time_limit: Option<Duration>,
    killer_moves: KillerMoves,
    history_table: HistoryTable,
    nodes_since_check: u64,
}

impl Default for ChessEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl ChessEngine {
    pub fn new() -> Self {
        let tt_size = 1 << 20; // 1M entries
        Self {
            transposition_table: vec![None; tt_size],
            tt_size,
            tt_age: 0,
            stats: SearchStats::default(),
            start_time: None,
            time_limit: None,
            killer_moves: KillerMoves::default(),
            history_table: HistoryTable::default(),
            nodes_since_check: 0,
        }
    }

    pub fn search(&mut self, board: &Board, depth: u8, time_limit: Option<Duration>) -> (ChessMove, i32) {
        self.stats = SearchStats::default();
        self.start_time = Some(Instant::now());
        self.time_limit = time_limit;
        self.tt_age = self.tt_age.wrapping_add(1);
        self.nodes_since_check = 0;
        
        let mut best_move = None;
        let mut best_score = -30000;

        // Iterative deepening
        for current_depth in 1.. {
            if let Some(time_limit) = time_limit {
                if let Some(start) = self.start_time {
                    if start.elapsed() >= time_limit {
                        break;
                    }
                }
            } else {
                if current_depth > depth {
                    break;
                }
            }

            let (move_found, score) = self.search_root(board, current_depth);
            
            if let Some(mv) = move_found {
                best_move = Some(mv);
                best_score = score;
                
                // Print search info
                if let Some(start) = self.start_time {
                    let elapsed = start.elapsed().as_millis();
                    let nps = if elapsed > 0 {
                        (self.stats.nodes_searched * 1000) / elapsed as u64
                    } else {
                        0
                    };
                    
                    println!(
                        "info depth {} score cp {} nodes {} time {} nps {} pv {}",
                        current_depth, score, self.stats.nodes_searched, elapsed, nps, mv
                    );
                }
            }

            // If we found a mate, no need to search deeper
            if score.abs() > 29000 {
                break;
            }
        }

        (best_move.unwrap_or_else(|| {
            // Fallback: return any legal move
            MoveGen::new_legal(board).next().expect("No legal moves available")
        }), best_score)
    }

    fn search_root(&mut self, board: &Board, depth: u8) -> (Option<ChessMove>, i32) {
        let mut best_move = None;
        let mut best_score = -30000;
        let mut alpha = -30000;
        let beta = 30000;

        let moves = self.order_moves(board, None, 0);
        
        for (i, chess_move) in moves.iter().enumerate() {
            let new_board = board.make_move_new(*chess_move);
            let mut score;

            if i == 0 {
                // First move: full window search
                score = -self.negamax(&new_board, depth - 1, -beta, -alpha, 1, true);
            } else {
                // Principal Variation Search
                score = -self.negamax(&new_board, depth - 1, -alpha - 1, -alpha, 1, true);
                
                // If PVS search fails high, re-search with full window
                if score > alpha && score < beta {
                    score = -self.negamax(&new_board, depth - 1, -beta, -alpha, 1, true);
                }
            }

            if score > best_score {
                best_score = score;
                best_move = Some(*chess_move);
            }

            alpha = alpha.max(score);
        }

        (best_move, best_score)
    }

    fn negamax(&mut self, board: &Board, depth: u8, mut alpha: i32, beta: i32, ply: usize, do_null: bool) -> i32 {
        self.stats.nodes_searched += 1;
        self.nodes_since_check += 1;

        // Check transposition table
        let hash = board.get_hash();
        let tt_index = (hash as usize) % self.tt_size;
        let hash_move = if let Some(ref entry) = self.transposition_table[tt_index] {
            if entry.key == (hash >> 32) as u32 && entry.depth >= depth {
                self.stats.transposition_hits += 1;
                let score = self.adjust_mate_score(entry.score as i32, ply);
                
                match entry.node_type() {
                    NodeType::Exact => return score,
                    NodeType::LowerBound => {
                        if score >= beta {
                            return score;
                        }
                        alpha = alpha.max(score);
                    }
                    NodeType::UpperBound => {
                        if score <= alpha {
                            return score;
                        }
                    }
                }
            }
            
            // Extract hash move for move ordering
            if entry.key == (hash >> 32) as u32 {
                self.unpack_move(entry.best_move)
            } else {
                None
            }
        } else {
            None
        };

        // Terminal node evaluation
        match board.status() {
            BoardStatus::Checkmate => return -29000 + ply as i32, // Prefer faster mates
            BoardStatus::Stalemate => return 0,
            BoardStatus::Ongoing => {}
        }

        // Leaf node evaluation
        if depth == 0 {
            return self.quiescence_search(board, alpha, beta, ply);
        }

        // Null move pruning
        if do_null && depth >= 3 && board.checkers().popcnt() == 0 && self.has_non_pawn_material(board) {
            let null_board = board.null_move().unwrap_or(*board);
            let null_score = -self.negamax(&null_board, depth - 3, -beta, -beta + 1, ply + 1, false);
            
            if null_score >= beta {
                self.stats.null_move_cutoffs += 1;
                return beta;
            }
        }

        let original_alpha = alpha;
        let mut best_move = None;
        let mut best_score = -30000;
        let mut move_count = 0;

        let moves = self.order_moves(board, hash_move, ply);
        
        for chess_move in moves {
            move_count += 1;
            let new_board = board.make_move_new(chess_move);
            let mut score;

            // Late move reductions
            if move_count > 4 && depth >= 3 && 
               board.piece_on(chess_move.get_dest()).is_none() && // Not a capture
               chess_move.get_promotion().is_none() && // Not a promotion
               new_board.checkers().popcnt() == 0 { // Doesn't give check
                
                // Reduce depth for late moves
                let reduction = if move_count > 8 { 2 } else { 1 };
                score = -self.negamax(&new_board, depth - 1 - reduction, -alpha - 1, -alpha, ply + 1, true);
                
                // If reduced search fails high, re-search with full depth
                if score > alpha {
                    score = -self.negamax(&new_board, depth - 1, -alpha - 1, -alpha, ply + 1, true);
                }
            } else if move_count == 1 {
                // First move: full window search
                score = -self.negamax(&new_board, depth - 1, -beta, -alpha, ply + 1, true);
            } else {
                // Principal Variation Search
                score = -self.negamax(&new_board, depth - 1, -alpha - 1, -alpha, ply + 1, true);
                
                // If PVS search fails high, re-search with full window
                if score > alpha && score < beta {
                    score = -self.negamax(&new_board, depth - 1, -beta, -alpha, ply + 1, true);
                }
            }

            if score > best_score {
                best_score = score;
                best_move = Some(chess_move);
            }

            alpha = alpha.max(score);

            // Beta cutoff
            if alpha >= beta {
                self.stats.beta_cutoffs += 1;
                
                // Update killer moves and history for quiet moves
                if board.piece_on(chess_move.get_dest()).is_none() {
                    self.killer_moves.add_killer(ply, chess_move);
                    self.history_table.add_history(board.side_to_move(), chess_move, depth);
                }
                break;
            }
        }

        // Store in transposition table with better replacement scheme
        if best_move.is_some() {
            let node_type = if best_score <= original_alpha {
                2 // UpperBound
            } else if best_score >= beta {
                1 // LowerBound
            } else {
                0 // Exact
            };

            let should_replace = if let Some(ref existing) = self.transposition_table[tt_index] {
                existing.key != (hash >> 32) as u32 || // Different position
                existing.depth <= depth || // Deeper search
                existing.age != self.tt_age // Older entry
            } else {
                true
            };

            if should_replace {
                self.transposition_table[tt_index] = Some(TranspositionEntry {
                    key: (hash >> 32) as u32,
                    best_move: self.pack_move(best_move.unwrap()),
                    depth,
                    score: self.adjust_mate_score_for_storage(best_score, ply) as i16,
                    node_type,
                    age: self.tt_age,
                });
            }
        }

        best_score
    }

    fn quiescence_search(&mut self, board: &Board, mut alpha: i32, beta: i32, ply: usize) -> i32 {
        self.stats.nodes_searched += 1;

        if ply > 20 {
            return self.evaluate_position(board);
        }

        let stand_pat = self.evaluate_position(board);
        
        if stand_pat >= beta {
            return beta;
        }

        // Delta pruning - if we're too far behind, don't bother
        if stand_pat < alpha - 900 {
            return alpha;
        }

        alpha = alpha.max(stand_pat);

        // Only consider captures and promotions in quiescence search
        let moves: Vec<ChessMove> = MoveGen::new_legal(board)
            .filter(|&mv| {
                board.piece_on(mv.get_dest()).is_some() || // Captures
                mv.get_promotion().is_some() // Promotions
            })
            .collect();

        let ordered_moves = self.order_captures(board, moves);

        for chess_move in ordered_moves {
            // SEE pruning - skip obviously bad captures
            if self.see(board, chess_move) < 0 {
                continue;
            }

            let new_board = board.make_move_new(chess_move);
            let score = -self.quiescence_search(&new_board, -beta, -alpha, ply + 1);

            if score >= beta {
                return beta;
            }

            alpha = alpha.max(score);
        }

        alpha
    }

    fn order_moves(&self, board: &Board, hash_move: Option<ChessMove>, ply: usize) -> Vec<ChessMove> {
        let mut moves: Vec<ChessMove> = MoveGen::new_legal(board).collect();
        
        // Score moves for ordering
        moves.sort_by_key(|&mv| {
            let mut score = 0;

            // Hash move gets highest priority
            if Some(mv) == hash_move {
                return -10000;
            }

            // Captures with SEE
            if let Some(captured_piece) = board.piece_on(mv.get_dest()) {
                let see_score = self.see(board, mv);
                if see_score >= 0 {
                    let victim_value = self.piece_value(captured_piece);
                    score -= 8000 + victim_value;
                } else {
                    score -= see_score; // Bad captures get negative score
                }
            }

            // Promotions
            if let Some(promotion) = mv.get_promotion() {
                score -= 7000 + self.piece_value(promotion);
            }

            // Killer moves
            if self.killer_moves.is_killer(ply, mv) {
                score -= 5000;
            }

            // History heuristic
            score -= self.history_table.get_history(board.side_to_move(), mv) / 10;

            // Checks
            let new_board = board.make_move_new(mv);
            if new_board.checkers().popcnt() > 0 {
                score -= 100;
            }

            score
        });

        moves
    }

    fn order_captures(&self, board: &Board, mut moves: Vec<ChessMove>) -> Vec<ChessMove> {
        moves.sort_by_key(|&mv| {
            let mut score = 0;

            if let Some(captured_piece) = board.piece_on(mv.get_dest()) {
                let see_score = self.see(board, mv);
                if see_score >= 0 {
                    let victim_value = self.piece_value(captured_piece);
                    score -= victim_value;
                } else {
                    score -= see_score;
                }
            }

            if let Some(promotion) = mv.get_promotion() {
                score -= self.piece_value(promotion);
            }

            score
        });

        moves
    }

    // Static Exchange Evaluation - simplified version
    fn see(&self, board: &Board, chess_move: ChessMove) -> i32 {
        let to = chess_move.get_dest();
        let _from = chess_move.get_source();
        
        let mut gain = 0;
        
        if let Some(captured_piece) = board.piece_on(to) {
            gain += self.piece_value(captured_piece);
        }
        
        if let Some(promotion) = chess_move.get_promotion() {
            gain += self.piece_value(promotion) - 100; // Pawn becomes promotion piece
        }
        
        // Simplified - just return the immediate gain
        // A full SEE would require analyzing the exchange sequence
        gain
    }

    fn has_non_pawn_material(&self, board: &Board) -> bool {
        let color = board.side_to_move();
        let pieces = board.color_combined(color);
        let pawns = board.pieces(Piece::Pawn) & pieces;
        let king = board.pieces(Piece::King) & pieces;
        
        (pieces ^ pawns ^ king).popcnt() > 0
    }

    fn piece_value(&self, piece: Piece) -> i32 {
        match piece {
            Piece::Pawn => 100,
            Piece::Knight => 320,
            Piece::Bishop => 330,
            Piece::Rook => 500,
            Piece::Queen => 900,
            Piece::King => 10000,
        }
    }

    fn evaluate_position(&self, board: &Board) -> i32 {
        let mut score = evaluate_board(board) as i32;
        
        // Flip score for black to move
        if board.side_to_move() == Color::Black {
            score = -score;
        }
        
        score
    }

    // Helper functions for transposition table
    fn pack_move(&self, chess_move: ChessMove) -> u16 {
        let from = chess_move.get_source().to_index() as u16;
        let to = chess_move.get_dest().to_index() as u16;
        let promotion = match chess_move.get_promotion() {
            Some(Piece::Queen) => 1,
            Some(Piece::Rook) => 2,
            Some(Piece::Bishop) => 3,
            Some(Piece::Knight) => 4,
            _ => 0,
        };
        
        from | (to << 6) | (promotion << 12)
    }

    fn unpack_move(&self, packed: u16) -> Option<ChessMove> {
        let from_idx = (packed & 0x3F) as u8;
        let to_idx = ((packed >> 6) & 0x3F) as u8;
        
        if from_idx >= 64 || to_idx >= 64 {
            return None;
        }
        
        let from = unsafe { Square::new(from_idx) };
        let to = unsafe { Square::new(to_idx) };
        let promotion_bits = (packed >> 12) & 0xF;
        
        let promotion = match promotion_bits {
            1 => Some(Piece::Queen),
            2 => Some(Piece::Rook),
            3 => Some(Piece::Bishop),
            4 => Some(Piece::Knight),
            _ => None,
        };
        
        Some(ChessMove::new(from, to, promotion))
    }

    fn adjust_mate_score(&self, score: i32, ply: usize) -> i32 {
        if score > 29000 {
            score - ply as i32
        } else if score < -29000 {
            score + ply as i32
        } else {
            score
        }
    }

    fn adjust_mate_score_for_storage(&self, score: i32, ply: usize) -> i32 {
        if score > 29000 {
            score + ply as i32
        } else if score < -29000 {
            score - ply as i32
        } else {
            score
        }
    }

    pub fn clear_transposition_table(&mut self) {
        for entry in &mut self.transposition_table {
            *entry = None;
        }
    }
}

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
const NAME: &str = env!("CARGO_PKG_NAME");

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

fn calculate_time_limit(
    wtime: Option<u64>,
    btime: Option<u64>,
    movestogo: Option<u64>,
    movetime: Option<u64>,
    side_to_move: Color,
) -> Option<Duration> {
    // Case 1: movetime override
    if let Some(ms) = movetime {
        return Some(Duration::from_millis(ms));
    }

    // Time remaining for the current player
    let time_left = match side_to_move {
        Color::White => wtime?,
        Color::Black => btime?,
    };

    // Parameters
    let reserve_fraction = 0.1; // Leave 10% of time as reserve
    let moves_remaining = movestogo.unwrap_or(40).max(1);

    // Avoid spending all time in one move
    let usable_time = (time_left as f64) * (1.0 - reserve_fraction);

    // More sophisticated time distribution: spend more time in earlier moves
    let time_per_move = (usable_time / (moves_remaining as f64).sqrt()).min(time_left as f64 * 0.9);

    Some(Duration::from_millis(time_per_move as u64))
}

fn main() {
    let mut engine = ChessEngine::new();
    let mut current_board = Board::default();

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        match parse(input) {
            Ok(("", UCICommand::Uci)) => {
                println!("id name {NAME} {VERSION}");
                println!("id author {AUTHORS}");
                println!("uciok");
            }
            Ok((_, UCICommand::IsReady)) => {
                println!("readyok");
            }
            Ok((_, UCICommand::Position { fen, moves })) => {
                if let Some(fen_string) = fen {
                    current_board = Board::from_str(&fen_string)
                        .unwrap_or_else(|_| {
                            eprintln!("Invalid FEN: {}", fen_string);
                            Board::default()
                        });
                } else {
                    current_board = Board::default();
                }

                for move_str in moves {
                    match uci_to_move(&move_str) {
                        Ok(chess_move) => {
                            current_board = current_board.make_move_new(chess_move);
                        }
                        Err(e) => {
                            eprintln!("Invalid move {}: {}", move_str, e);
                            break;
                        }
                    }
                }
            }
            Ok((
                _,
                UCICommand::Go {
                    wtime,
                    btime,
                    movestogo,
                    movetime,
                    depth,
                    nodes: _,
                },
            )) => {
                let search_depth = depth.unwrap_or(10).min(25) as u8; // Increased default depth
                let time_limit = calculate_time_limit(
                    wtime, btime, movestogo, movetime, current_board.side_to_move()
                );

                let (best_move, _score) = engine.search(&current_board, search_depth, time_limit);
                println!("bestmove {}", best_move);
                
                // More aggressive transposition table cleanup
                if engine.transposition_table.iter().filter(|e| e.is_some()).count() > 800_000 {
                    let current_age = engine.tt_age;
                    engine.transposition_table.iter_mut().for_each(|entry| {
                        if let Some(ref e) = entry {
                            if e.age != current_age && e.depth < 4 {
                                *entry = None;
                            }
                        }
                    });
                }
            }
            Ok((_, UCICommand::Quit)) => break,
            _ => {
                // Handle unknown commands silently or with minimal response
            }
        }
    }
}
