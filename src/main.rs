use chess::*;
use rand::Rng;
use std::fmt::{self, Debug, Formatter};

struct Node {
    board: Board,
    total_score: i64,
    number_of_visits: u64,
    first_child: Option<Box<Node>>,
    next_sibling: Option<Box<Node>>,
    move_to_reach: Option<ChessMove>, // Add move_to_reach field
}

impl Default for Node {
    fn default() -> Self {
        Self {
            board: Board::default(),
            total_score: 0,
            number_of_visits: 0,
            first_child: None,
            next_sibling: None,
            move_to_reach: None, // Initialize move_to_reach
        }
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "")?;
        self.fmt_with_indent(f, 0) // Start formatting with zero indentation
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
        // Write the current node with indentation
        writeln!(
            f,
            "{:indent$}Node t:{}, n:{} ({})",
            "",
            self.total_score,
            self.number_of_visits,
            self.board,
            indent = indent * 4 // Indent by 4 spaces per level
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

/// Find the child with the highest UCB1 score
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

fn evaluate_board(board: &Board) -> i64 {
    // A simple heuristic evaluation function
    // Positive values favor White, negative values favor Black
    let mut score = 0;

    for square in 0..64 {
        let square = unsafe { Square::new(square) };
        if let Some(piece) = board.piece_on(square) {
            let piece_value = match piece {
                Piece::Pawn => 1,
                Piece::Knight | Piece::Bishop => 3,
                Piece::Rook => 5,
                Piece::Queen => 9,
                Piece::King => 0, // King's value is not considered in this simple heuristic
            };

            if board.color_on(square) == Some(Color::White) {
                score += piece_value;
            } else {
                score -= piece_value;
            }
        }
    }

    score
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
            panic!("No legal moves")
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

pub const VERSION: &'static str = env!("CARGO_PKG_VERSION");
pub const AUTHORS: &'static str = env!("CARGO_PKG_AUTHORS");
pub const NAME: &'static str = env!("CARGO_PKG_NAME");

fn main() {
    let mut root = Node::default();
    // Preprocessing the root node
    for _ in 0..1_000 {
        mcts(&mut root);
    }

    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        match input {
            "uci" => {
                println!("id name {} {}", NAME, VERSION);
                println!("id author {}", AUTHORS);
                println!("uciok");
            }
            "isready" => {
                println!("readyok");
            }
            "position" => todo!(),
            "go" => todo!(),
            "quit" => break,
            "setoption" => todo!(),
            "ucinewgame" => todo!(),
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
