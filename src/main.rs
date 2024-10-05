use chess::*;
use rand::Rng;
use std::{
    fmt::{self, Debug, Formatter},
    str::FromStr,
};

#[derive(Clone)]
struct Node {
    board: Board,
    total_score: i64,
    number_of_visits: u64,
    first_child: Option<Box<Node>>,
    next_sibling: Option<Box<Node>>,
    move_to_reach: Option<ChessMove>,
}

impl Default for Node {
    fn default() -> Self {
        Self {
            board: Board::default(),
            total_score: 0,
            number_of_visits: 0,
            first_child: None,
            next_sibling: None,
            move_to_reach: None,
        }
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "")?;
        self.fmt_with_indent(f, 0)
    }
}

impl Node {
    fn new(board: Board, move_to_reach: Option<ChessMove>) -> Self {
        Self {
            board,
            move_to_reach,
            ..Default::default()
        }
    }

    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
        writeln!(
            f,
            "{:indent$}Node t:{}, n:{} ({})",
            "",
            self.total_score,
            self.number_of_visits,
            self.board,
            indent = indent * 4
        )?;

        // Recursively format the first child
        if let Some(ref first_child) = self.first_child {
            first_child.fmt_with_indent(f, indent + 1)?;
        }

        // Recursively format the next sibling at the same level
        if let Some(ref next_sibling) = self.next_sibling {
            next_sibling.fmt_with_indent(f, indent)?;
        }

        Ok(())
    }

    fn add_child(&mut self, child: Node) {
        if let Some(first_child) = &mut self.first_child {
            first_child.add_sibling(child);
        } else {
            self.first_child = Some(Box::new(child));
        }
    }

    fn add_sibling(&mut self, sibling: Node) {
        if let Some(next_sibling) = &mut self.next_sibling {
            next_sibling.add_sibling(sibling);
        } else {
            self.next_sibling = Some(Box::new(sibling));
        }
    }

    fn is_leaf(&self) -> bool {
        self.first_child.is_none()
    }
}

/// Performs a Monte Carlo Tree Search on the given root node
/// and generates the tree from the root node.
fn mcts(root: &mut Node) {
    let mut ancestors = vec![root as *mut Node];
    let mut current: *mut Node = root;

    unsafe {
        while !(*current).is_leaf() {
            // Find child with highest UCB1 score
            // And set that child as the current node
            let mut best_ucb1 = f64::NEG_INFINITY;

            // Use raw pointers to avoid borrowing issues
            let mut current_child = (*current).first_child.as_deref_mut().unwrap() as *mut Node;
            let mut best_child = current_child;

            while let Some(current_child_ref) = current_child.as_mut() {
                let ucb1 = ucb1(current_child_ref, &*current);
                if ucb1 > best_ucb1 {
                    best_ucb1 = ucb1;
                    best_child = current_child;
                }

                if let Some(next_sibling) = current_child_ref.next_sibling.as_deref_mut() {
                    current_child = next_sibling as *mut Node;
                } else {
                    break;
                }
            }

            current = best_child;
            ancestors.push(current);
        }

        if (*current).number_of_visits != 0 || (*current).board == (*ancestors[0]).board {
            for move_ in MoveGen::new_legal(&(*current).board) {
                let new_board_state = (*current).board.make_move_new(move_);
                let new_node = Node::new(new_board_state, Some(move_));
                (*current).add_child(new_node);
            }
        }

        let rollout_score = rollout(&*current);

        // Backpropagate the score
        for &ancestor in ancestors.iter().rev() {
            (*ancestor).total_score += rollout_score;
            (*ancestor).number_of_visits += 1;
        }
    }
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

fn evaluate_board(board: &Board) -> i64 {
    let is_white_turn = board.side_to_move() == Color::White;
    // Checkmate condition
    if board.status() == BoardStatus::Checkmate {
        return if is_white_turn {
            -20000 as i64
        } else {
            20000 as i64
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

    score as i64
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

fn rollout(current: &Node) -> i64 {
    // Simulate a game from the current node
    // And return the score
    let mut board = current.board;
    let mut rng = rand::thread_rng();
    let mut depth = 0;
    let max_depth = 100; // Set a maximum depth for the rollout

    loop {
        match board.status() {
            BoardStatus::Checkmate => {
                return if board.side_to_move() == Color::White {
                    1
                } else {
                    -1
                };
            }
            BoardStatus::Stalemate => return 0,
            _ => {}
        }

        if depth >= max_depth {
            return evaluate_board(&board); // Use heuristic evaluation at max depth
        }

        let moves = MoveGen::new_legal(&board).collect::<Vec<_>>();
        if moves.is_empty() {
            unreachable!("No legal moves")
        }
        let random_move = moves[rng.gen_range(0..moves.len())];
        board = board.make_move_new(random_move);
        depth += 1;
    }
}

fn ucb1(child: &Node, parent: &Node) -> f64 {
    if child.number_of_visits == 0 {
        return f64::INFINITY;
    }

    let exploitation = (child.total_score as f64) / (child.number_of_visits as f64);
    let exploration = (parent.number_of_visits as f64).ln() / (child.number_of_visits as f64);
    let exploration_term = exploration.sqrt();
    let c = 1.414; // Exploration constant
    exploitation + c * exploration_term
}

fn find_best_move(root: &Node) -> Option<ChessMove> {
    let mut best_child: Option<&Node> = None;
    let mut max_visits = 0;

    let mut current_child = root.first_child.as_deref();
    while let Some(child) = current_child {
        if child.number_of_visits > max_visits {
            max_visits = child.number_of_visits;
            best_child = Some(child);
        }
        current_child = child.next_sibling.as_deref();
    }

    best_child.and_then(|child| child.move_to_reach)
}

fn find_fen_in_tree(node: &mut Node, target_board: Board) -> bool {
    // WARN: I have to check if this actually works
    if node.board == target_board {
        return true;
    }

    let mut current_child = node.first_child.as_deref_mut();
    while let Some(child) = current_child {
        if find_fen_in_tree(child, target_board) {
            return true;
        }
        current_child = child.next_sibling.as_deref_mut();
    }

    false
}

fn apply_or_add_move(node: &mut Node, chess_move: ChessMove) {
    let new_board = node.board.make_move_new(chess_move);

    let mut current_child = node.first_child.as_deref_mut();
    while let Some(child) = current_child {
        if child.board == new_board {
            // Move found in the tree, update the current node to this child's state
            *node = child.clone(); // Now this should work if Clone is implemented correctly
            return;
        }
        current_child = child.next_sibling.as_deref_mut();
    }

    // Move not found, add a new child node and update the current node
    let new_node = Node::new(new_board, Some(chess_move));
    node.add_child(new_node);

    // Update the current node to the new state
    *node = *(*node.first_child.as_ref().unwrap()).clone(); // Correct dereferencing
}

const VERSION: &'static str = env!("CARGO_PKG_VERSION");
const AUTHORS: &'static str = env!("CARGO_PKG_AUTHORS");
const NAME: &'static str = env!("CARGO_PKG_NAME");

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

fn main() {
    let mut root = Node::default();

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();
        let command = input.split_whitespace().next().unwrap_or("");

        match command {
            "uci" => {
                println!("id name {} {}", NAME, VERSION);
                println!("id author {}", AUTHORS);
                println!("uciok");
            }
            "isready" => {
                println!("readyok");
            }
            "position" => {
                let mut parts = input.split_whitespace().skip(1); // Skip "position"
                match parts.next() {
                    Some("startpos") => {
                        root = Node::default(); // Reset to starting position
                        if let Some("moves") = parts.next() {
                            for move_str in parts {
                                let chess_move = uci_to_move(move_str).expect("Invalid move");
                                apply_or_add_move(&mut root, chess_move);
                            }
                        }
                    }
                    Some("fen") => {
                        let fen = parts.by_ref().take(6).collect::<Vec<&str>>().join(" ");
                        let new_board = Board::from_str(&fen).expect("Invalid FEN string");

                        // Try to find the FEN position in the existing MCTS tree
                        if !find_fen_in_tree(&mut root, new_board) {
                            // If FEN not found, start fresh from this new position.
                            root = Node::new(new_board, None); // New root node from FEN
                        } else {
                            // If FEN found, we might want to add any subsequent moves
                            if let Some("moves") = parts.next() {
                                for move_str in parts {
                                    let chess_move = uci_to_move(move_str).expect("Invalid move");
                                    apply_or_add_move(&mut root, chess_move);
                                }
                            }
                        }
                    }
                    _ => panic!("Unknown position type"),
                }
            }
            "go" => {
                // Perform MCTS to find the best move
                for _ in 0..5_000 {
                    mcts(&mut root);
                }

                if let Some(best_move) = find_best_move(&root) {
                    println!("bestmove {}", best_move);
                } else {
                    println!("bestmove (none)");
                }
            }
            "quit" => break,
            "setoption" => todo!(),
            "ucinewgame" => {
                root = Node::default(); // Reset the root nodes
            }
            "stop" => todo!(),
            "ponderhit" => todo!(),
            _ => unimplemented!("Unknown command"),
        }
    }

    // dbg!(&root);
    // if let Some(best_move) = find_best_move(&root) {
    //     println!("Best move found: {}", best_move);
    // } else {
    //     println!("No best move found.");
    // }
}
