use chess::*;
use rayon::prelude::*;
use std::{
    fmt::Debug,
    str::FromStr,
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    sync::{Arc, Mutex},
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
    nodes_searched: AtomicU64,
    transposition_hits: AtomicU64,
    beta_cutoffs: AtomicU64,
    null_move_cutoffs: AtomicU64,
}

pub struct ChessEngine {
    transposition_table: Arc<Mutex<Vec<Option<TranspositionEntry>>>>,
    tt_size: usize,
    tt_age: u8,
    stats: SearchStats,
    start_time: Option<Instant>,
    time_limit: Option<Duration>,
    killer_moves: KillerMoves,
    history_table: HistoryTable,
    stop_search: Arc<AtomicBool>,
    contempt: i32, // To avoid draws when winning
}

impl Default for ChessEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl ChessEngine {
    pub fn new() -> Self {
        let tt_size = 1 << 22; // 4M entries for deeper search
        Self {
            transposition_table: Arc::new(Mutex::new(vec![None; tt_size])),
            tt_size,
            tt_age: 0,
            stats: SearchStats::default(),
            start_time: None,
            time_limit: None,
            killer_moves: KillerMoves::default(),
            history_table: HistoryTable::default(),
            stop_search: Arc::new(AtomicBool::new(false)),
            contempt: 20, // Small contempt factor to avoid draws
        }
    }

    pub fn search(&mut self, board: &Board, depth: u8, time_limit: Option<Duration>) -> (ChessMove, i32) {
        self.stats = SearchStats::default();
        self.start_time = Some(Instant::now());
        self.time_limit = time_limit;
        self.tt_age = self.tt_age.wrapping_add(1);
        self.stop_search.store(false, Ordering::Relaxed);
        
        let mut best_move = None;
        let mut best_score = -30000;

        // Increased default depth to 12, maximum to 50
        let max_depth = depth.max(12).min(50);

        // Iterative deepening
        for current_depth in 1..=max_depth {
            if self.time_up() {
                break;
            }

            let (move_found, score) = self.search_root(board, current_depth);
            
            if let Some(mv) = move_found {
                best_move = Some(mv);
                best_score = score;
                
                // Print search info
                if let Some(start) = self.start_time {
                    let elapsed = start.elapsed().as_millis().max(1);
                    let nodes = self.stats.nodes_searched.load(Ordering::Relaxed);
                    let nps = (nodes * 1000) / elapsed as u64;
                    
                    println!(
                        "info depth {} score cp {} nodes {} time {} nps {} pv {}",
                        current_depth, score, nodes, elapsed, nps, mv
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
        let alpha = -30000;
        let beta = 30000;

        let moves = self.order_moves(board, None, 0);
        
        // Parallel search for root moves when depth is sufficient
        if depth >= 4 && moves.len() > 1 {
            let results: Vec<_> = moves
                .par_iter()
                .enumerate()
                .map(|(i, &chess_move)| {
                    if self.stop_search.load(Ordering::Relaxed) {
                        return (chess_move, -30000);
                    }

                    let new_board = board.make_move_new(chess_move);
                    let mut local_engine = self.clone_for_parallel();
                    
                    let score = if i == 0 {
                        // First move: full window search
                        -local_engine.negamax(&new_board, depth - 1, -beta, -alpha, 1, true)
                    } else {
                        // Principal Variation Search
                        let mut score = -local_engine.negamax(&new_board, depth - 1, -alpha - 1, -alpha, 1, true);
                        
                        // If PVS search fails high, re-search with full window
                        if score > alpha && score < beta && !self.stop_search.load(Ordering::Relaxed) {
                            score = -local_engine.negamax(&new_board, depth - 1, -beta, -alpha, 1, true);
                        }
                        score
                    };

                    (chess_move, score)
                })
                .collect();

            // Find best result
            for (chess_move, score) in results {
                if score > best_score {
                    best_score = score;
                    best_move = Some(chess_move);
                }
            }
        } else {
            // Sequential search for shallow depths or single moves
            let mut alpha_local = alpha;
            
            for (i, chess_move) in moves.iter().enumerate() {
                if self.time_up() {
                    break;
                }

                let new_board = board.make_move_new(*chess_move);
                let mut score;

                if i == 0 {
                    // First move: full window search
                    score = -self.negamax(&new_board, depth - 1, -beta, -alpha_local, 1, true);
                } else {
                    // Principal Variation Search
                    score = -self.negamax(&new_board, depth - 1, -alpha_local - 1, -alpha_local, 1, true);
                    
                    // If PVS search fails high, re-search with full window
                    if score > alpha_local && score < beta && !self.time_up() {
                        score = -self.negamax(&new_board, depth - 1, -beta, -alpha_local, 1, true);
                    }
                }

                if score > best_score {
                    best_score = score;
                    best_move = Some(*chess_move);
                }

                alpha_local = alpha_local.max(score);
            }
        }

        (best_move, best_score)
    }

    fn clone_for_parallel(&self) -> ChessEngine {
        ChessEngine {
            transposition_table: self.transposition_table.clone(),
            tt_size: self.tt_size,
            tt_age: self.tt_age,
            stats: SearchStats::default(),
            start_time: self.start_time,
            time_limit: self.time_limit,
            killer_moves: KillerMoves::default(), // Each thread gets its own killer moves
            history_table: HistoryTable::default(), // Each thread gets its own history
            stop_search: self.stop_search.clone(),
            contempt: self.contempt,
        }
    }

    fn negamax(&mut self, board: &Board, depth: u8, mut alpha: i32, beta: i32, ply: usize, do_null: bool) -> i32 {
        self.stats.nodes_searched.fetch_add(1, Ordering::Relaxed);

        // Periodic time check
        if self.stats.nodes_searched.load(Ordering::Relaxed) % 50_000 == 0 {
            if self.time_up() {
                self.stop_search.store(true, Ordering::Relaxed);
                return 0;
            }
        }

        if self.stop_search.load(Ordering::Relaxed) {
            return 0;
        }

        // Check transposition table
        let hash = board.get_hash();
        let tt_index = (hash as usize) % self.tt_size;
        let hash_move = {
            let tt_guard = self.transposition_table.lock().unwrap();
            if let Some(ref entry) = tt_guard[tt_index] {
                if entry.key == (hash >> 32) as u32 && entry.depth >= depth {
                    self.stats.transposition_hits.fetch_add(1, Ordering::Relaxed);
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
            }
        };

        // Terminal node evaluation
        match board.status() {
            BoardStatus::Checkmate => return -29000 + ply as i32,
            BoardStatus::Stalemate => {
                // Apply contempt factor to avoid draws when winning  
                let eval = self.evaluate_position(board);
                if eval > 50 {
                    return -self.contempt; // Avoid draws when we're ahead
                } else if eval < -50 {
                    return self.contempt; // Accept draws when we're behind
                }
                return 0;
            }
            BoardStatus::Ongoing => {}
        }

        // Check for threefold repetition with contempt
        if self.is_repetition(board) {
            let eval = self.evaluate_position(board);
            if eval > 50 {
                return -self.contempt;
            } else if eval < -50 {
                return self.contempt;
            }
            return 0;
        }

        // Leaf node evaluation
        if depth == 0 {
            return self.quiescence_search(board, alpha, beta, ply);
        }

        // Null move pruning with increased reduction
        if do_null && depth >= 3 && board.checkers().popcnt() == 0 && self.has_non_pawn_material(board) {
            let null_board = board.null_move().unwrap_or(*board);
            let reduction = if depth >= 6 { 4 } else { 3 }; // More aggressive null move
            let null_score = -self.negamax(&null_board, depth - reduction, -beta, -beta + 1, ply + 1, false);
            
            if null_score >= beta {
                self.stats.null_move_cutoffs.fetch_add(1, Ordering::Relaxed);
                return beta;
            }
        }

        let original_alpha = alpha;
        let mut best_move = None;
        let mut best_score = -30000;
        let mut move_count = 0;

        let moves = self.order_moves(board, hash_move, ply);
        
        for chess_move in moves {
            if self.stop_search.load(Ordering::Relaxed) {
                break;
            }

            move_count += 1;
            let new_board = board.make_move_new(chess_move);
            let mut score;

            // More aggressive late move reductions
            if move_count > 3 && depth >= 3 && 
               board.piece_on(chess_move.get_dest()).is_none() && // Not a capture
               chess_move.get_promotion().is_none() && // Not a promotion
               new_board.checkers().popcnt() == 0 && // Doesn't give check
               !self.killer_moves.is_killer(ply, chess_move) { // Not a killer move
                
                // More aggressive reduction based on move count and depth
                let reduction = if move_count > 8 && depth >= 6 { 
                    3 
                } else if move_count > 6 { 
                    2 
                } else { 
                    1 
                };
                
                score = -self.negamax(&new_board, depth - 1 - reduction, -alpha - 1, -alpha, ply + 1, true);
                
                // If reduced search fails high, re-search with full depth
                if score > alpha && !self.stop_search.load(Ordering::Relaxed) {
                    score = -self.negamax(&new_board, depth - 1, -alpha - 1, -alpha, ply + 1, true);
                }
            } else if move_count == 1 {
                // First move: full window search
                score = -self.negamax(&new_board, depth - 1, -beta, -alpha, ply + 1, true);
            } else {
                // Principal Variation Search
                score = -self.negamax(&new_board, depth - 1, -alpha - 1, -alpha, ply + 1, true);
                
                // If PVS search fails high, re-search with full window
                if score > alpha && score < beta && !self.stop_search.load(Ordering::Relaxed) {
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
                self.stats.beta_cutoffs.fetch_add(1, Ordering::Relaxed);
                
                // Update killer moves and history for quiet moves
                if board.piece_on(chess_move.get_dest()).is_none() {
                    self.killer_moves.add_killer(ply, chess_move);
                    self.history_table.add_history(board.side_to_move(), chess_move, depth);
                }
                break;
            }
        }

        // Store in transposition table
        if best_move.is_some() && !self.stop_search.load(Ordering::Relaxed) {
            let node_type = if best_score <= original_alpha {
                2 // UpperBound
            } else if best_score >= beta {
                1 // LowerBound
            } else {
                0 // Exact
            };

            let mut tt_guard = self.transposition_table.lock().unwrap();
            let should_replace = if let Some(ref existing) = tt_guard[tt_index] {
                existing.key != (hash >> 32) as u32 || // Different position
                existing.depth <= depth || // Deeper search
                existing.age != self.tt_age // Older entry
            } else {
                true
            };

            if should_replace {
                tt_guard[tt_index] = Some(TranspositionEntry {
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
        self.stats.nodes_searched.fetch_add(1, Ordering::Relaxed);

        if ply > 30 || self.stop_search.load(Ordering::Relaxed) {
            return self.evaluate_position(board);
        }

        let stand_pat = self.evaluate_position(board);
        
        if stand_pat >= beta {
            return beta;
        }

        // More aggressive delta pruning
        if stand_pat < alpha - 1200 {  // Increased from 900
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
            if self.stop_search.load(Ordering::Relaxed) {
                break;
            }

            // SEE pruning - skip obviously bad captures
            if self.see(board, chess_move) < -100 {  // More lenient SEE pruning
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

    fn is_repetition(&self, board: &Board) -> bool {
        // Simplified repetition detection
        // In a real implementation, you'd maintain a game history
        false
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

            // Captures with MVV-LVA and SEE
            if let Some(captured_piece) = board.piece_on(mv.get_dest()) {
                let see_score = self.see(board, mv);
                if see_score >= 0 {
                    let victim_value = self.piece_value(captured_piece);
                    let attacker_value = board.piece_on(mv.get_source())
                        .map(|p| self.piece_value(p))
                        .unwrap_or(0);
                    score -= 8000 + victim_value - attacker_value / 10; // MVV-LVA
                } else {
                    score -= see_score;
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

            // Checks (giving check is good)
            let new_board = board.make_move_new(mv);
            if new_board.checkers().popcnt() > 0 {
                score -= 200;
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
                    let attacker_value = board.piece_on(mv.get_source())
                        .map(|p| self.piece_value(p))
                        .unwrap_or(0);
                    score -= victim_value - attacker_value / 10; // MVV-LVA
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

    // Improved Static Exchange Evaluation
    fn see(&self, board: &Board, chess_move: ChessMove) -> i32 {
        let to = chess_move.get_dest();
        let from = chess_move.get_source();
        
        let mut gain = 0;
        
        // Value of captured piece
        if let Some(captured_piece) = board.piece_on(to) {
            gain += self.piece_value(captured_piece);
        }
        
        // Value gained from promotion
        if let Some(promotion) = chess_move.get_promotion() {
            gain += self.piece_value(promotion) - 100; // Pawn becomes promotion piece
        }
        
        // Value lost (attacking piece)
        if let Some(attacking_piece) = board.piece_on(from) {
            // Simple approximation: assume we lose the attacking piece
            // A full SEE would analyze the entire exchange sequence
            let attackers_to_square = board.color_combined(!board.side_to_move()) & 
                (board.pieces(Piece::Pawn) | board.pieces(Piece::Knight) | 
                 board.pieces(Piece::Bishop) | board.pieces(Piece::Rook) | 
                 board.pieces(Piece::Queen));
            
            if (attackers_to_square & BitBoard::from_square(to)).popcnt() > 0 {
                gain -= self.piece_value(attacking_piece);
            }
        }
        
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

    fn time_up(&self) -> bool {
        if let (Some(start), Some(limit)) = (self.start_time, self.time_limit) {
            start.elapsed() >= limit
        } else {
            false
        }
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
        let mut tt_guard = self.transposition_table.lock().unwrap();
        for entry in tt_guard.iter_mut() {
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
    if let Some(ms) = movetime {
        return Some(Duration::from_millis(ms));
    }

    let time_left = match side_to_move {
        Color::White => wtime,
        Color::Black => btime,
    }?;

    let moves_remaining = movestogo.unwrap_or(30).max(1); // More aggressive time usage
    let buffer_divisor = moves_remaining + 2; // Slightly more conservative buffer

    // Use more time per move for deeper search
    let time_per_move = time_left / buffer_divisor;
    Some(Duration::from_millis(time_per_move.max(100))) // Minimum 100ms per move
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
                println!("option name Contempt type spin default 20 min -100 max 100");
                println!("option name Threads type spin default 1 min 1 max 128");
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
                // Increased default depth to 15, maximum to 50
                let search_depth = depth.unwrap_or(15).min(50) as u8;
                let time_limit = calculate_time_limit(
                    wtime, btime, movestogo, movetime, current_board.side_to_move()
                );

                let (best_move, _score) = engine.search(&current_board, search_depth, time_limit);
                println!("bestmove {}", best_move);
                
                // More aggressive transposition table cleanup for deeper searches
                {
                    let tt_guard = engine.transposition_table.lock().unwrap();
                    let filled_entries = tt_guard.iter().filter(|e| e.is_some()).count();
                    drop(tt_guard);
                    
                    if filled_entries > (engine.tt_size * 3 / 4) {
                        let current_age = engine.tt_age;
                        let mut tt_guard = engine.transposition_table.lock().unwrap();
                        tt_guard.iter_mut().for_each(|entry| {
                            if let Some(ref e) = entry {
                                if e.age != current_age && e.depth < 6 {
                                    *entry = None;
                                }
                            }
                        });
                    }
                }
            }
            Ok((_, UCICommand::Quit)) => break,
            _ => {
                // Handle unknown commands silently or with minimal response
            }
        }
    }
}
