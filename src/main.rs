use chess::*;
use std::{
    collections::HashMap,
    fmt::Debug,
    str::FromStr,
    time::{Duration, Instant},
};

mod evaluate;
use evaluate::evaluate_board;

mod uci;
use uci::*;

const CHECKMATE_SCORE: i16 = 25000;
const DRAW_SCORE: i16 = -30;

// Optimized transposition table entry - packed into 12 bytes
#[derive(Clone, Debug)]
struct TranspositionEntry {
    key: u32,       // Upper 32 bits of hash for verification
    best_move: u16, // Packed move representation
    depth: u8,
    score: i16,
    node_type: NodeType,
    age: u8,
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
        if ply >= 64 {
            return;
        }

        // Don't add the same move twice
        if self.moves[ply][0] == Some(chess_move) {
            return;
        }

        // Shift moves
        self.moves[ply][1] = self.moves[ply][0];
        self.moves[ply][0] = Some(chess_move);
    }

    fn is_killer(&self, ply: usize, chess_move: ChessMove) -> bool {
        if ply >= 64 {
            return false;
        }
        self.moves[ply][0] == Some(chess_move) || self.moves[ply][1] == Some(chess_move)
    }
}

// History heuristic for move ordering
#[derive(Debug)]
struct HistoryTable {
    table: [[[i16; 64]; 64]; 2], // [color][from][to]
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

        let bonus = (depth as i16) * (depth as i16);
        self.table[color_idx][from][to] += bonus;

        // Prevent overflow
        if self.table[color_idx][from][to] > 10000 {
            self.age_history();
        }
    }

    fn get_history(&self, color: Color, chess_move: ChessMove) -> i16 {
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

// Position history for repetition detection
#[derive(Debug, Default)]
struct PositionHistory {
    positions: HashMap<u64, u8>, // hash -> count
    history: Vec<u64>,           // Stack of position hashes for easy rollback
    move_count: u16,             // Track moves for 50-move rule
    last_capture_or_pawn: u16,   // Last move where capture or pawn move occurred
}

impl PositionHistory {
    fn push_position(&mut self, hash: u64, is_capture_or_pawn: bool) {
        self.history.push(hash);
        *self.positions.entry(hash).or_insert(0) += 1;
        self.move_count += 1;

        if is_capture_or_pawn {
            self.last_capture_or_pawn = self.move_count;
        }
    }

    fn pop_position(&mut self) {
        if let Some(hash) = self.history.pop() {
            if let Some(count) = self.positions.get_mut(&hash) {
                *count -= 1;
                if *count == 0 {
                    self.positions.remove(&hash);
                }
            }
            if self.move_count > 0 {
                self.move_count -= 1;
            }
        }
    }

    fn get_repetition_count(&self, hash: u64) -> u8 {
        self.positions.get(&hash).copied().unwrap_or(0)
    }

    fn is_threefold_repetition(&self, hash: u64) -> bool {
        self.get_repetition_count(hash) >= 3
    }

    fn is_fifty_move_rule(&self) -> bool {
        self.move_count.saturating_sub(self.last_capture_or_pawn) >= 100
    }

    fn clear(&mut self) {
        self.positions.clear();
        self.history.clear();
        self.move_count = 0;
        self.last_capture_or_pawn = 0;
    }
}

#[derive(Debug, Default)]
struct SearchStats {
    nodes_searched: u64,
    transposition_hits: u64,
    beta_cutoffs: u64,
    null_move_cutoffs: u64,
    repetition_draws: u64,
}

pub struct ChessEngine {
    // Use a vector for better cache performance
    transposition_table: Vec<Option<TranspositionEntry>>,
    tt_size: usize,
    tt_age: u8,
    stats: SearchStats,
    start_time: Option<Instant>,
    killer_moves: KillerMoves,
    history_table: HistoryTable,
    nodes_since_check: u64,
    position_history: PositionHistory,
    stop: bool,
    time_control: TimeControl,
}

impl Default for ChessEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl ChessEngine {
    pub fn new() -> Self {
        let tt_size = 1 << 20; // 8M entries
        Self {
            transposition_table: vec![None; tt_size],
            tt_size,
            tt_age: 0,
            stats: SearchStats::default(),
            start_time: None,
            killer_moves: KillerMoves::default(),
            history_table: HistoryTable::default(),
            nodes_since_check: 0,
            position_history: PositionHistory::default(),
            stop: false,
            time_control: TimeControl {
                optimum: Duration::MAX,
                maximum: Duration::MAX,
            },
        }
    }

    pub fn set_position(&mut self, board: &Board) {
        // Clear position history when setting a new position
        self.position_history.clear();
        self.position_history.push_position(board.get_hash(), false);
    }

    pub fn make_move(&mut self, board: &Board, chess_move: ChessMove) -> Board {
        let new_board = board.make_move_new(chess_move);

        // Check if this move is a capture or pawn move (resets 50-move counter)
        let is_capture_or_pawn = board.piece_on(chess_move.get_dest()).is_some()
            || board.piece_on(chess_move.get_source()) == Some(Piece::Pawn);

        self.position_history
            .push_position(new_board.get_hash(), is_capture_or_pawn);
        new_board
    }

    pub fn unmake_move(&mut self) {
        self.position_history.pop_position();
    }

    pub fn search(
        &mut self,
        board: &Board,
        depth: Option<u8>,
        time_control: TimeControl,
    ) -> ChessMove {
        let max_depth = depth.unwrap_or(u8::MAX);

        self.stats = SearchStats::default();
        self.start_time = Some(Instant::now());
        self.stop = false;
        self.nodes_since_check = 0;

        let start = self.start_time.unwrap();

        let mut pv: Vec<ChessMove> = vec![];
        let mut best_move = None;

        let mut last_score: Option<i16> = None;
        let mut best_move_stable_iters = 0;
        let mut aspiration_alpha = -CHECKMATE_SCORE;
        let mut aspiration_beta = CHECKMATE_SCORE;

        for current_depth in 1..=max_depth {
            // Use aspiration window from depth 4 onwards
            if current_depth >= 4 {
                if let Some(prev) = last_score {
                    aspiration_alpha = prev - 50;
                    aspiration_beta = prev + 50;
                }
            } else {
                aspiration_alpha = -CHECKMATE_SCORE;
                aspiration_beta = CHECKMATE_SCORE;
            }

            let mut score =
                self.search_root(board, current_depth, aspiration_alpha, aspiration_beta);

            // If score fell outside the window, re-search with full window
            if score <= aspiration_alpha || score >= aspiration_beta {
                let wide_alpha = aspiration_alpha - 150;
                let wide_beta = aspiration_beta + 150;
                score = self.search_root(board, current_depth, wide_alpha, wide_beta);
                // If still outside, then do full window
                if score <= wide_alpha || score >= wide_beta {
                    score =
                        self.search_root(board, current_depth, -CHECKMATE_SCORE, CHECKMATE_SCORE);
                }
            }
            if self.stop {
                break;
            }

            pv = self.get_pv(*board);

            if pv.is_empty() {
                break;
            }

            best_move = Some(pv[0]);

            // Stability tracking
            if let Some(prev) = last_score {
                if (prev - score).abs() < 20 {
                    best_move_stable_iters += 1;
                } else {
                    best_move_stable_iters = 0;
                }
            }

            last_score = Some(score);

            let elapsed = start.elapsed();

            let pv_str = pv
                .iter()
                .map(|m| m.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            let elapsed_ms = elapsed.as_millis();
            let nps = if elapsed_ms > 0 {
                (self.stats.nodes_searched * 1000) / elapsed_ms as u64
            } else {
                0
            };

            let score_str = self.format_score(score);
            let hashfull = self
                .transposition_table
                .iter()
                .filter(|e| e.is_some())
                .count()
                * 1000
                / self.tt_size;

            println!(
                "info depth {} score {} nodes {} time {} nps {} hashfull {} pv {}",
                current_depth,
                score_str,
                self.stats.nodes_searched,
                elapsed_ms,
                nps,
                hashfull,
                pv_str
            );

            // HARD STOP
            if elapsed >= time_control.maximum {
                break;
            }

            // Found a forced mate within search depth
            if score > CHECKMATE_SCORE - 1000 {
                let mate_in_moves = (CHECKMATE_SCORE - score + 1) / 2;
                if mate_in_moves <= current_depth as i16 {
                    break;
                }
            }

            // SOFT STOP
            if elapsed >= time_control.optimum {
                let score_stable = best_move_stable_iters >= 3;
                let score_dropped = last_score.map_or(false, |prev| score < prev - 20);
                let score_spiked = last_score.map_or(false, |prev| score > prev + 20);

                // If score is dropping or spiking, the position is dynamic - use more time
                if score_dropped || score_spiked {
                    // Allow up to maximum, don't stop early
                    continue;
                }

                if score_stable {
                    break;
                }

                // Default: stop at optimum if not clearly unstable
                break;
            }
        }

        best_move.expect("No legal move found")
    }

    fn is_draw(&self, board: &Board) -> bool {
        // Check for threefold repetition
        if self
            .position_history
            .is_threefold_repetition(board.get_hash())
        {
            return true;
        }

        // Check for 50-move rule
        if self.position_history.is_fifty_move_rule() {
            return true;
        }

        // Check for insufficient material
        self.is_insufficient_material(board)
    }

    fn is_insufficient_material(&self, board: &Board) -> bool {
        let white_pieces = board.color_combined(Color::White);
        let black_pieces = board.color_combined(Color::Black);

        let white_pawns = (board.pieces(Piece::Pawn) & white_pieces).popcnt();
        let black_pawns = (board.pieces(Piece::Pawn) & black_pieces).popcnt();
        let white_rooks = (board.pieces(Piece::Rook) & white_pieces).popcnt();
        let black_rooks = (board.pieces(Piece::Rook) & black_pieces).popcnt();
        let white_queens = (board.pieces(Piece::Queen) & white_pieces).popcnt();
        let black_queens = (board.pieces(Piece::Queen) & black_pieces).popcnt();
        let white_bishops = (board.pieces(Piece::Bishop) & white_pieces).popcnt();
        let black_bishops = (board.pieces(Piece::Bishop) & black_pieces).popcnt();
        let white_knights = (board.pieces(Piece::Knight) & white_pieces).popcnt();
        let black_knights = (board.pieces(Piece::Knight) & black_pieces).popcnt();

        // If there are pawns, rooks, or queens, material is sufficient
        if white_pawns > 0
            || black_pawns > 0
            || white_rooks > 0
            || black_rooks > 0
            || white_queens > 0
            || black_queens > 0
        {
            return false;
        }

        let white_minor = white_bishops + white_knights;
        let black_minor = black_bishops + black_knights;

        // King vs King
        if white_minor == 0 && black_minor == 0 {
            return true;
        }

        // King + minor piece vs King
        if (white_minor == 1 && black_minor == 0) || (white_minor == 0 && black_minor == 1) {
            return true;
        }

        // King + Bishop vs King + Bishop (same colored squares)
        if white_bishops == 1 && black_bishops == 1 && white_knights == 0 && black_knights == 0 {
            let white_bishop_sq = (board.pieces(Piece::Bishop) & white_pieces).to_square();
            let black_bishop_sq = (board.pieces(Piece::Bishop) & black_pieces).to_square();

            // Check if bishops are on same color squares
            let white_on_light =
                (white_bishop_sq.get_file().to_index() + white_bishop_sq.get_rank().to_index()) % 2
                    == 0;
            let black_on_light =
                (black_bishop_sq.get_file().to_index() + black_bishop_sq.get_rank().to_index()) % 2
                    == 0;

            if white_on_light == black_on_light {
                return true;
            }
        }

        false
    }

    fn get_pv(&self, mut board: Board) -> Vec<ChessMove> {
        let mut pv = Vec::with_capacity(64);

        for _ in 0..64 {
            let hash = board.get_hash();
            let tt_index = (hash as usize) % self.tt_size;

            if let Some(ref entry) = self.transposition_table[tt_index] {
                if entry.key == (hash >> 32) as u32 {
                    if let Some(mv) = self.unpack_move(entry.best_move) {
                        pv.push(mv);
                        board = board.make_move_new(mv);
                        continue; // keep following
                    }
                }
            }
            break; // stop if no continuation found
        }

        pv
    }

    fn format_score(&self, score: i16) -> String {
        const MATE_THRESHOLD: i16 = CHECKMATE_SCORE - 1000;

        if score > MATE_THRESHOLD {
            // Positive mate score - we're winning
            let mate_in = (CHECKMATE_SCORE - score + 1) / 2;
            format!("mate {}", mate_in)
        } else if score < -MATE_THRESHOLD {
            // Negative mate score - we're getting mated
            let mate_in = (CHECKMATE_SCORE + score + 1) / 2;
            format!("mate -{}", mate_in)
        } else {
            // Regular centipawn score
            format!("cp {}", score)
        }
    }

    fn search_root(&mut self, board: &Board, depth: u8, alpha: i16, beta: i16) -> i16 {
        let mut best_move = None;
        let mut best_score = -CHECKMATE_SCORE;
        let mut alpha = alpha;
        let original_alpha = alpha;

        // Check for immediate draw
        if self.is_draw(board) {
            return DRAW_SCORE;
        }

        let moves = self.order_moves(board, None, 0);

        for (i, chess_move) in moves.iter().enumerate() {
            if self.stop {
                break;
            }

            let new_board = board.make_move_new(*chess_move);
            let is_capture_or_pawn = board.piece_on(chess_move.get_dest()).is_some()
                || board.piece_on(chess_move.get_source()) == Some(Piece::Pawn);
            self.position_history
                .push_position(new_board.get_hash(), is_capture_or_pawn);

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

            self.position_history.pop_position();

            if score > best_score {
                best_score = score;
                best_move = Some(*chess_move);
            }

            alpha = alpha.max(score);

            if alpha >= beta {
                break;
            }
        }

        let hash = board.get_hash();
        let tt_index = (hash as usize) % self.tt_size;
        if let Some(best_move) = best_move {
            let node_type = if best_score <= original_alpha {
                NodeType::UpperBound
            } else if best_score >= beta {
                NodeType::LowerBound
            } else {
                NodeType::Exact
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
                    best_move: self.pack_move(best_move),
                    depth,
                    score: self.adjust_mate_score_for_storage(best_score, 0),
                    node_type,
                    age: self.tt_age,
                });
            }
        }

        best_score
    }

    fn material_eval(&self, board: &Board) -> i16 {
        let mut score = 0i16;
        for piece in [
            Piece::Pawn,
            Piece::Knight,
            Piece::Bishop,
            Piece::Rook,
            Piece::Queen,
        ] {
            let value = self.piece_value(piece);
            let white = (board.pieces(piece) & board.color_combined(Color::White)).popcnt() as i16;
            let black = (board.pieces(piece) & board.color_combined(Color::Black)).popcnt() as i16;
            score += (white - black) * value;
        }
        score *= match board.side_to_move() {
            Color::White => 1,
            Color::Black => -1,
        };
        score
    }

    fn negamax(
        &mut self,
        board: &Board,
        depth: u8,
        alpha: i16,
        beta: i16,
        ply: usize,
        do_null: bool,
    ) -> i16 {
        self.stats.nodes_searched += 1;
        self.nodes_since_check += 1;
        if self.nodes_since_check >= 2048 {
            self.nodes_since_check = 0;

            if let Some(start) = self.start_time {
                if start.elapsed() >= self.time_control.maximum {
                    self.stop = true;
                }
            }
        }

        if self.stop {
            return 0;
        }

        // Check for draw conditions first
        if self.is_draw(board) {
            self.stats.repetition_draws += 1;
            return DRAW_SCORE;
        }

        // Mate distance pruning
        let mut alpha = alpha.max(-(CHECKMATE_SCORE - ply as i16 - 1));
        let beta = beta.min(CHECKMATE_SCORE - ply as i16);
        if alpha >= beta {
            return alpha;
        }

        // Check transposition table
        let hash = board.get_hash();

        let tt_index = (hash as usize) % self.tt_size;
        let hash_move = if let Some(ref entry) = self.transposition_table[tt_index] {
            if entry.key == (hash >> 32) as u32 && entry.depth >= depth && entry.age == self.tt_age
            {
                self.stats.transposition_hits += 1;
                let score = self.adjust_mate_score(entry.score, ply);

                match entry.node_type {
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
            BoardStatus::Checkmate => return -CHECKMATE_SCORE + ply as i16,
            BoardStatus::Stalemate => return DRAW_SCORE,
            BoardStatus::Ongoing => {}
        }

        // Leaf node evaluation
        if depth == 0 {
            return self.quiescence_search(board, alpha, beta, ply);
        }

        // Reverse futility pruning (static null move pruning)
        let in_check = board.checkers().popcnt() > 0;

        // Only compute static eval if we might actually use it for pruning
        let static_eval = if !in_check && depth <= 6 {
            Some(self.material_eval(board))
        } else {
            None
        };

        // Reverse futility pruning
        if let Some(se) = static_eval {
            if depth <= 6
                && !in_check
                && self.has_non_pawn_material(board)
                && se - 80 * depth as i16 >= beta
            {
                return se - 80 * depth as i16;
            }
        }

        // Futility pruning flag
        let futility_pruning = match static_eval {
            Some(se) => depth <= 2 && !in_check && se + 75 * depth as i16 <= alpha,
            None => false,
        };

        // Null move pruning
        if do_null
            && depth >= 3
            && board.checkers().popcnt() == 0
            && self.has_non_pawn_material(board)
        {
            if let Some(null_board) = board.null_move() {
                let is_capture_or_pawn = false; // Null moves are never captures or pawn moves
                self.position_history
                    .push_position(null_board.get_hash(), is_capture_or_pawn);
                let null_score =
                    -self.negamax(&null_board, depth - 3, -beta, -beta + 1, ply + 1, false);
                self.position_history.pop_position();

                if null_score >= beta {
                    self.stats.null_move_cutoffs += 1;
                    return beta;
                }
            }
        }

        // Internal iterative deepening
        let mut hash_move = hash_move; // make mutable
        if hash_move.is_none() && depth >= 4 {
            self.negamax(board, depth - 2, alpha, beta, ply, false);
            if let Some(ref entry) = self.transposition_table[tt_index] {
                if entry.key == (hash >> 32) as u32 {
                    hash_move = self.unpack_move(entry.best_move);
                }
            }
        }

        let original_alpha = alpha;
        let mut best_move = None;
        let mut best_score = -CHECKMATE_SCORE;
        let mut move_count = 0;

        let moves = self.order_moves(board, hash_move, ply);

        for chess_move in moves {
            move_count += 1;
            if futility_pruning
                && board.piece_on(chess_move.get_dest()).is_none()
                && chess_move.get_promotion().is_none()
                && !self.killer_moves.is_killer(ply, chess_move)
            {
                continue;
            }

            let new_board = board.make_move_new(chess_move);
            let is_capture_or_pawn = board.piece_on(chess_move.get_dest()).is_some()
                || board.piece_on(chess_move.get_source()) == Some(Piece::Pawn);
            self.position_history
                .push_position(new_board.get_hash(), is_capture_or_pawn);

            // Late move pruning - at low depths skip late quiet moves entirely
            if depth <= 3
                && move_count > 3 + (depth as usize * depth as usize)
                && !in_check
                && board.piece_on(chess_move.get_dest()).is_none()
                && chess_move.get_promotion().is_none()
                && new_board.checkers().popcnt() == 0
            {
                self.position_history.pop_position();
                continue;
            }

            let mut score;
            let gives_check = new_board.checkers().popcnt() > 0;
            let extension = if gives_check { 1i8 } else { 0i8 };

            // Late move reductions
            if move_count > 4
                && depth >= 3
                && board.piece_on(chess_move.get_dest()).is_none()
                && chess_move.get_promotion().is_none()
                && !gives_check
            // replaces the popcnt() == 0 check
            {
                let reduction = (0.5 + (depth as f64).ln() * (move_count as f64).ln() / 2.0) as i8;
                let reduction = reduction.clamp(1, 4);
                let new_depth = (depth as i8 - 1 + extension - reduction).max(0) as u8;
                score = -self.negamax(&new_board, new_depth, -alpha - 1, -alpha, ply + 1, true);

                if score > alpha {
                    let full_depth = (depth as i8 - 1 + extension).max(0) as u8;
                    score = -self.negamax(&new_board, full_depth, -beta, -alpha, ply + 1, true);
                }
            } else if move_count == 1 {
                score = -self.negamax(
                    &new_board,
                    (depth as i8 - 1 + extension).max(0) as u8,
                    -beta,
                    -alpha,
                    ply + 1,
                    true,
                );
            } else {
                score = -self.negamax(
                    &new_board,
                    (depth as i8 - 1 + extension).max(0) as u8,
                    -alpha - 1,
                    -alpha,
                    ply + 1,
                    true,
                );

                if score > alpha && score < beta {
                    score = -self.negamax(
                        &new_board,
                        (depth as i8 - 1 + extension).max(0) as u8,
                        -beta,
                        -alpha,
                        ply + 1,
                        true,
                    );
                }
            }

            self.position_history.pop_position();

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
                    self.history_table
                        .add_history(board.side_to_move(), chess_move, depth);
                }
                break;
            }
        }

        // Store in transposition table with better replacement scheme
        if let Some(best_move) = best_move {
            let node_type = if best_score <= original_alpha {
                NodeType::UpperBound
            } else if best_score >= beta {
                NodeType::LowerBound
            } else {
                NodeType::Exact
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
                    best_move: self.pack_move(best_move),
                    depth,
                    score: self.adjust_mate_score_for_storage(best_score, ply),
                    node_type,
                    age: self.tt_age,
                });
            }
        }

        best_score
    }

    fn quiescence_search(&mut self, board: &Board, mut alpha: i16, beta: i16, ply: usize) -> i16 {
        self.stats.nodes_searched += 1;
        self.nodes_since_check += 1;

        if self.nodes_since_check >= 2048 {
            self.nodes_since_check = 0;

            if let Some(start) = self.start_time {
                if start.elapsed() >= self.time_control.maximum {
                    self.stop = true;
                }
            }
        }

        if self.stop {
            return 0;
        }

        // Check for draw in quiescence as well
        if self.is_draw(board) {
            return DRAW_SCORE;
        }

        let mut board_evaluation = evaluate_board(board);
        board_evaluation *= match board.side_to_move() {
            Color::White => 1,
            Color::Black => -1,
        };

        if ply > 20 {
            return board_evaluation;
        }

        let stand_pat = board_evaluation;
        if stand_pat >= beta {
            return beta;
        }

        // Delta pruning - if we're too far behind, don't bother
        if stand_pat < alpha - 300 {
            return alpha;
        }

        alpha = alpha.max(stand_pat);

        let moves: Vec<ChessMove> = MoveGen::new_legal(board)
            .filter(|&mv| {
                board.piece_on(mv.get_dest()).is_some() // Captures
        || mv.get_promotion().is_some()                 // Promotions
        || (ply < 2 && {                                // Checks in first 2 plies only
            let new_board = board.make_move_new(mv);
            new_board.checkers().popcnt() > 0
        })
            })
            .collect();

        let ordered_moves = self.sort_moves_by_see(board, moves);

        for chess_move in ordered_moves {
            // SEE pruning - skip obviously bad captures
            if self.see(board, chess_move) < 0 {
                continue;
            }

            let new_board = board.make_move_new(chess_move);

            let is_capture_or_pawn = board.piece_on(chess_move.get_dest()).is_some()
                || board.piece_on(chess_move.get_source()) == Some(Piece::Pawn);
            self.position_history
                .push_position(new_board.get_hash(), is_capture_or_pawn);
            let score = -self.quiescence_search(&new_board, -beta, -alpha, ply + 1);
            self.position_history.pop_position();

            if score >= beta {
                return beta;
            }

            alpha = alpha.max(score);
        }

        alpha
    }

    fn order_moves(
        &self,
        board: &Board,
        hash_move: Option<ChessMove>,
        ply: usize,
    ) -> Vec<ChessMove> {
        let moves: Vec<ChessMove> = MoveGen::new_legal(board).collect();

        // Pre-compute values that don't change per move
        let side_to_move = board.side_to_move();

        // Score all moves first (avoiding repeated computations in sort)
        let mut scored_moves: Vec<(ChessMove, i16)> = moves
            .into_iter()
            .map(|mv| {
                let mut score = 0;

                // Hash move gets highest priority
                if Some(mv) == hash_move {
                    return (mv, -10000);
                }

                let dest = mv.get_dest();
                // Captures with SEE
                if let Some(captured_piece) = board.piece_on(dest) {
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

                // Killer moves (check this before history for better cache locality)
                if self.killer_moves.is_killer(ply, mv) {
                    score -= 5000;
                }

                // History heuristic (pre-divided to avoid division in sort)
                score -= self.history_table.get_history(side_to_move, mv) / 10;

                (mv, score)
            })
            .collect();

        Self::sort_moves_by_score(&mut scored_moves);

        // Extract just the moves
        scored_moves.into_iter().map(|(mv, _)| mv).collect()
    }

    #[inline]
    fn sort_moves_by_score(arr: &mut [(ChessMove, i16)]) {
        for i in 1..arr.len() {
            let key = arr[i];
            let mut j = i;

            // Safety: j > 0 initially and decrements, bounds are guaranteed
            while j > 0 && unsafe { arr.get_unchecked(j - 1) }.1 > key.1 {
                unsafe {
                    *arr.get_unchecked_mut(j) = *arr.get_unchecked_mut(j - 1);
                }
                j -= 1;
            }
            arr[j] = key;
        }
    }

    fn sort_moves_by_see(&self, board: &Board, mut moves: Vec<ChessMove>) -> Vec<ChessMove> {
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
    fn see(&self, board: &Board, chess_move: ChessMove) -> i16 {
        let to = chess_move.get_dest();
        let from = chess_move.get_source();

        // Initial gain is the captured piece value
        let mut gain = [0i16; 32];
        let mut depth = 0;

        gain[0] = if let Some(captured) = board.piece_on(to) {
            self.piece_value(captured)
        } else if chess_move.get_promotion().is_some() {
            0
        } else {
            return 0; // Not a capture or promotion, SEE = 0
        };

        // Handle promotion: the moving piece becomes the promotion piece
        let mut attacker_value = if let Some(promotion) = chess_move.get_promotion() {
            gain[0] += self.piece_value(promotion) - self.piece_value(Piece::Pawn);
            self.piece_value(promotion)
        } else {
            self.piece_value(board.piece_on(from).unwrap())
        };

        // Build occupancy bitboard - start with all pieces, remove as they're used
        let mut occupied = *board.combined();
        // Remove the moving piece
        occupied &= !BitBoard::from_square(from);

        // Track which side is capturing next
        let mut side = board.side_to_move();
        side = !side; // After the first capture, it's the opponent's turn

        // Get all attackers of the target square
        let mut attackers = self.get_all_attackers(board, to, occupied);
        // Remove the piece we just moved from attackers
        attackers &= !BitBoard::from_square(from);

        loop {
            depth += 1;
            if depth >= 32 {
                break;
            }

            // Find least valuable attacker for current side
            let (lva_square, lva_piece) =
                match self.get_least_valuable_attacker(board, attackers, side, occupied) {
                    Some(x) => x,
                    None => break, // No more attackers for this side
                };

            // Assume we capture, gain is previous gain minus attacker value
            gain[depth] = attacker_value - gain[depth - 1];
            attacker_value = self.piece_value(lva_piece);

            // Remove this attacker from occupied and attackers
            occupied &= !BitBoard::from_square(lva_square);
            attackers &= !BitBoard::from_square(lva_square);

            // Add any x-ray attackers revealed by removing this piece
            attackers |= self.get_xray_attackers(board, to, lva_square, occupied);

            // Switch sides
            side = !side;
        }

        // Negamax the gain array
        // Work backwards - each side will only capture if it improves their score
        for i in (1..depth).rev() {
            gain[i - 1] = -(-gain[i - 1]).max(gain[i]);
        }

        gain[0]
    }

    fn get_all_attackers(&self, board: &Board, square: Square, occupied: BitBoard) -> BitBoard {
        let mut attackers = BitBoard(0);

        // Pawn attackers (white pawns attacking from below, black from above)
        let white_pawn_attacks = chess::get_pawn_attacks(square, Color::Black, occupied);
        attackers |=
            white_pawn_attacks & board.pieces(Piece::Pawn) & board.color_combined(Color::White);

        let black_pawn_attacks = chess::get_pawn_attacks(square, Color::White, occupied);
        attackers |=
            black_pawn_attacks & board.pieces(Piece::Pawn) & board.color_combined(Color::Black);

        // Knight attackers
        let knight_attacks = chess::get_knight_moves(square);
        attackers |= knight_attacks & board.pieces(Piece::Knight);

        // King attackers
        let king_attacks = chess::get_king_moves(square);
        attackers |= king_attacks & board.pieces(Piece::King);

        // Bishop/Queen attackers (diagonal)
        let bishop_attacks = chess::get_bishop_moves(square, occupied);
        attackers |= bishop_attacks & (board.pieces(Piece::Bishop) | board.pieces(Piece::Queen));

        // Rook/Queen attackers (straight)
        let rook_attacks = chess::get_rook_moves(square, occupied);
        attackers |= rook_attacks & (board.pieces(Piece::Rook) | board.pieces(Piece::Queen));

        attackers
    }

    fn get_xray_attackers(
        &self,
        board: &Board,
        square: Square,
        removed: Square,
        occupied: BitBoard,
    ) -> BitBoard {
        let mut xray = BitBoard(0);

        // Only sliders can be revealed as x-ray attackers
        // Check if the removed piece was on a diagonal or straight line with the square
        let bishop_attacks = chess::get_bishop_moves(square, occupied);
        let rook_attacks = chess::get_rook_moves(square, occupied);

        // If removed piece was on a bishop line, look for newly revealed bishops/queens
        if bishop_attacks & BitBoard::from_square(removed) != BitBoard(0) {
            xray |= bishop_attacks & (board.pieces(Piece::Bishop) | board.pieces(Piece::Queen));
        }

        // If removed piece was on a rook line, look for newly revealed rooks/queens
        if rook_attacks & BitBoard::from_square(removed) != BitBoard(0) {
            xray |= rook_attacks & (board.pieces(Piece::Rook) | board.pieces(Piece::Queen));
        }

        xray
    }

    fn get_least_valuable_attacker(
        &self,
        board: &Board,
        attackers: BitBoard,
        side: Color,
        _occupied: BitBoard,
    ) -> Option<(Square, Piece)> {
        let side_attackers = attackers & board.color_combined(side);

        if side_attackers == BitBoard(0) {
            return None;
        }

        // Check piece types from least to most valuable
        for piece in [
            Piece::Pawn,
            Piece::Knight,
            Piece::Bishop,
            Piece::Rook,
            Piece::Queen,
            Piece::King,
        ] {
            let piece_attackers = side_attackers & board.pieces(piece);
            if piece_attackers != BitBoard(0) {
                let square = piece_attackers.to_square();
                return Some((square, piece));
            }
        }

        None
    }

    fn has_non_pawn_material(&self, board: &Board) -> bool {
        let color = board.side_to_move();
        let pieces = board.color_combined(color);
        let pawns = board.pieces(Piece::Pawn) & pieces;
        let king = board.pieces(Piece::King) & pieces;

        (pieces ^ pawns ^ king).popcnt() > 0
    }

    fn piece_value(&self, piece: Piece) -> i16 {
        match piece {
            Piece::Pawn => 100,
            Piece::Knight => 320,
            Piece::Bishop => 330,
            Piece::Rook => 500,
            Piece::Queen => 900,
            Piece::King => 10000,
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

    fn adjust_mate_score(&self, score: i16, ply: usize) -> i16 {
        if score > CHECKMATE_SCORE - 1000 {
            score - ply as i16
        } else if score < -CHECKMATE_SCORE + 1000 {
            score + ply as i16
        } else {
            score
        }
    }

    fn adjust_mate_score_for_storage(&self, score: i16, ply: usize) -> i16 {
        if score > CHECKMATE_SCORE - 1000 {
            score + ply as i16
        } else if score < -CHECKMATE_SCORE + 1000 {
            score - ply as i16
        } else {
            score
        }
    }

    pub fn clear_transposition_table(&mut self) {
        for entry in &mut self.transposition_table {
            *entry = None;
        }
        self.position_history.clear();
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

pub struct TimeControl {
    optimum: Duration,
    maximum: Duration,
}

fn calculate_time_limit(
    wtime: Option<u64>,
    btime: Option<u64>,
    winc: Option<u64>,
    binc: Option<u64>,
    movestogo: Option<u64>,
    movetime: Option<u64>,
    side_to_move: Color,
) -> TimeControl {
    if wtime.is_none() && btime.is_none() && movetime.is_none() {
        return TimeControl {
            optimum: Duration::MAX,
            maximum: Duration::MAX,
        };
    }

    if let Some(ms) = movetime {
        let t = ms.saturating_sub(20).max(1);
        return TimeControl {
            optimum: Duration::from_millis(t),
            maximum: Duration::from_millis(t),
        };
    }

    let (time_left, increment) = match side_to_move {
        Color::White => (wtime.unwrap_or(0), winc.unwrap_or(0)),
        Color::Black => (btime.unwrap_or(0), binc.unwrap_or(0)),
    };

    if time_left <= 100 {
        return TimeControl {
            optimum: Duration::from_millis(10),
            maximum: Duration::from_millis(15),
        };
    }

    if time_left <= 1000 {
        let t = (time_left / 8).max(10);
        return TimeControl {
            optimum: Duration::from_millis(t),
            maximum: Duration::from_millis((t * 2).min(time_left / 2)),
        };
    }

    let overhead = 50u64;
    let safe_time = time_left.saturating_sub(overhead);

    let (optimum, maximum) = if let Some(mtg) = movestogo {
        // Repeating time control (e.g. 40/300)
        // Distribute time evenly but keep a buffer for the last few moves
        let mtg = mtg.max(1);
        let base = safe_time / mtg;
        let inc_bonus = increment * 3 / 4;
        let opt = (base + inc_bonus).min(safe_time / 3);
        let max = (opt * 3).min(safe_time * 2 / 3);
        (opt, max)
    } else {
        // Sudden death - estimate moves remaining based on time left
        let moves_left = if time_left > 120_000 {
            40
        } else if time_left > 60_000 {
            35
        } else if time_left > 30_000 {
            25
        } else if time_left > 10_000 {
            20
        } else if time_left > 5_000 {
            15
        } else {
            10
        };

        let base = safe_time / moves_left;
        let inc_bonus = increment * 3 / 4;
        let opt = (base + inc_bonus).min(safe_time / 5); // Never more than 20% of remaining
        let max = (opt * 3).min(safe_time / 3); // Max is 3x optimum but never > 33%
        (opt, max)
    };

    TimeControl {
        optimum: Duration::from_millis(optimum.max(10)),
        maximum: Duration::from_millis(maximum.max(20)),
    }
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
            Ok((_, UCICommand::Eval)) => {
                let mut evaluation = evaluate_board(&current_board) as f64;
                evaluation /= 100.0;
                println!(
                    "Evaluation: {}{}",
                    if evaluation > 0.0 { "+" } else { "-" },
                    evaluation.abs()
                );
            }
            Ok((_, UCICommand::Display)) => {
                let mut result = String::new();

                // Top border
                result.push_str("  +---+---+---+---+---+---+---+---+\n");

                // Iterate through ranks (8 to 1, top to bottom)
                for rank in (0..8).rev() {
                    result.push_str(&format!("{} |", rank + 1)); // Rank numbers

                    for file in 0..8 {
                        let square = chess::Square::make_square(
                            chess::Rank::from_index(rank),
                            chess::File::from_index(file),
                        );

                        let piece_char = match current_board.piece_on(square) {
                            Some(piece) => {
                                let piece_symbol = match piece {
                                    chess::Piece::Pawn => 'P',
                                    chess::Piece::Rook => 'R',
                                    chess::Piece::Knight => 'N',
                                    chess::Piece::Bishop => 'B',
                                    chess::Piece::Queen => 'Q',
                                    chess::Piece::King => 'K',
                                };

                                // Use lowercase for black pieces, uppercase for white
                                if current_board.color_on(square) == Some(chess::Color::White) {
                                    piece_symbol
                                } else {
                                    piece_symbol.to_ascii_lowercase()
                                }
                            }
                            None => ' ',
                        };

                        result.push_str(&format!(" {piece_char} |"));
                    }

                    result.push('\n');
                    result.push_str("  +---+---+---+---+---+---+---+---+\n");
                }

                // File letters at bottom
                result.push_str("    a   b   c   d   e   f   g   h\n");

                // Add game info
                result.push_str(&format!(
                    "\nSide to move: {}\n",
                    if current_board.side_to_move() == chess::Color::White {
                        "White"
                    } else {
                        "Black"
                    }
                ));

                if let Some(ep_square) = current_board.en_passant() {
                    result.push_str(&format!("En passant: {ep_square}\n"));
                }

                let castling_rights = current_board.castle_rights(chess::Color::White);
                let black_castling = current_board.castle_rights(chess::Color::Black);
                if castling_rights != chess::CastleRights::NoRights
                    || black_castling != chess::CastleRights::NoRights
                {
                    result.push_str("Castling: ");
                    if castling_rights.has_kingside() {
                        result.push('K');
                    }
                    if castling_rights.has_queenside() {
                        result.push('Q');
                    }
                    if black_castling.has_kingside() {
                        result.push('k');
                    }
                    if black_castling.has_queenside() {
                        result.push('q');
                    }
                    result.push('\n');
                }

                println!("{result}");
            }
            Ok((_, UCICommand::Bench)) => {
                println!("Running benchmark...");
                let mut benchmark_engine = ChessEngine::new();
                // Engine: SF 17.1
                // info depth 30 seldepth 43 multipv 1 score cp -14 pv e5d4 c3d4 c6a5 b3c2 a5c4 e3c1 c7c5 b2b3 c4b6 b1d2 g4h5 h2h3 c5d4 g2g4 d4d3 c2d3 h5g6 f3d4 d6d5 d4c6 d8c7 c6e7 c7e7 e4d5 e7c5 d2f3 c5c3 c1e3 f6d5 d3f1 d5e3 e1e3 c3c7 a1c1 a8d8 d1e1
                // info depth 30 seldepth 48 multipv 2 score cp -17 pv d6d5 e4d5 e5d4 e3d4 c6d4 c3d4 e7b4 b1c3 f8e8 e1e8 d8e8 h2h3 g4f3 d1f3 b4d6 a2a4 a8b8 a4b5 a6b5 g2g3 e8e7 f3d3 e7d7 g1g2 g7g6 a1a6 b5b4 c3d1 h7h5
                // info depth 30 seldepth 39 multipv 3 score cp -22 pv g4h5 b1d2 d6d5 e3g5 e5d4 e4d5 d4c3 d5c6 c3d2 d1d2 h5f3 d2d8 a8d8 e1e7 f3c6 e7c7 c6d5 h2h3 d5b3 a2b3 d8d6 c7a7 h7h6 g5f4 d6d3 a1a6 d3b3 f4e5 f6e4 f2f3 e4g5
                // info depth 30 seldepth 42 multipv 4 score cp -26 pv c6a5 d4e5 g4f3 d1f3 d6e5 b1d2 a5b3 a2b3 d8c8 e3g5 h7h6 g5f6 e7f6 f3f5 c8e8 b3b4 e8c6 d2f1 g7g6 f5f3 g8g7 f1e3 f8d8 e1d1 h6h5 d1d8 a8d8 e3d5 f6g5 g2g3
                // info depth 30 seldepth 46 multipv 5 score cp -30 pv a6a5 d4d5 a5a4 b3c2 c6a5 h2h3 g4d7 b2b3 c7c6 f3e5 d6e5 d5d6 h7h6 d6e7 d8e7 b1d2 a4b3 a2b3 c6c5 d1b1 a5c6 a1a8 f8a8 c2d3 b5b4 c3b4 c5b4 b1b2 d7e6 e1c1 e7d7 d3e2
                // bestmove e5d4 ponder c3d4
                let benchmark_board = Board::from_str(
                    "r2q1rk1/2p1bppp/p1np1n2/1p2p3/3PP1b1/1BP1BN2/PP3PPP/RN1QR1K1 b - - 2 10",
                )
                .unwrap();
                benchmark_engine.set_position(&benchmark_board);
                let best_move = benchmark_engine.search(
                    &benchmark_board,
                    Some(10),
                    TimeControl {
                        optimum: Duration::from_secs(10),
                        maximum: Duration::from_secs(10),
                    },
                );
                println!("Best move: {best_move}");
            }
            Ok((_, UCICommand::Position { fen, moves })) => {
                if let Some(fen_string) = fen {
                    current_board = Board::from_str(&fen_string).unwrap_or_else(|_| {
                        eprintln!("Invalid FEN: {fen_string}");
                        Board::default()
                    });
                } else {
                    current_board = Board::default();
                }

                // Set up position history for the engine
                engine.set_position(&current_board);

                // Apply moves and track position history
                for move_str in moves {
                    match uci_to_move(&move_str) {
                        Ok(chess_move) => {
                            current_board = engine.make_move(&current_board, chess_move);
                        }
                        Err(e) => {
                            eprintln!("Invalid move {move_str}: {e}");
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
                    winc,
                    binc,
                    movestogo,
                    movetime,
                    depth,
                    nodes: _,
                },
            )) => {
                engine.tt_age = engine.tt_age.wrapping_add(1);
                let time_limit = calculate_time_limit(
                    wtime,
                    btime,
                    winc,
                    binc,
                    movestogo,
                    movetime,
                    current_board.side_to_move(),
                );

                let best_move = engine.search(&current_board, depth, time_limit);
                println!("bestmove {best_move}");
            }
            Ok((_, UCICommand::Quit)) => break,
            Ok((_, UCICommand::NewGame)) => {
                engine.clear_transposition_table();
                engine.tt_age = 0;
                engine.history_table = HistoryTable::default();
                engine.killer_moves = KillerMoves::default();
                engine.position_history.clear();
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_threefold_repetition() {
        let mut engine = ChessEngine::new();
        let mut board = Board::default();

        engine.set_position(&board);

        let moves = [
            "g1f3", "g8f6", "f3g1", "f6g8", "g1f3", "g8f6", "f3g1", "f6g8",
        ];

        for mv in moves {
            let m = uci_to_move(mv).unwrap();
            board = engine.make_move(&board, m);
        }

        assert!(engine.is_draw(&board));
    }

    #[test]
    fn test_fifty_move_rule() {
        let mut engine = ChessEngine::new();
        let mut board = Board::from_str("8/8/8/8/8/8/8/K1k5 w - - 0 1").unwrap();
        engine.set_position(&board);

        // 50 full moves = 100 halfmoves
        for _ in 0..100 {
            let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
            let mv = moves[0];
            board = engine.make_move(&board, mv);
        }

        assert!(engine.position_history.is_fifty_move_rule());
    }

    #[test]
    fn test_insufficient_material() {
        let engine = ChessEngine::new();
        let board = Board::from_str("8/8/8/8/8/8/8/K1k5 w - - 0 1").unwrap();
        assert!(engine.is_insufficient_material(&board));
    }
}
