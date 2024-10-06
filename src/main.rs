use chess::*;
use rand::Rng;
use std::time::{Duration, Instant};
use std::{
    fmt::{self, Debug, Formatter},
    str::FromStr,
};

mod evaluate;
use evaluate::evaluate_board;

mod uci;
use uci::*;

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
            "{:indent$}Node t:{}, n:{} ({}) ({})",
            "",
            self.total_score,
            self.number_of_visits,
            self.board,
            self.move_to_reach
                .as_ref()
                .map(|m| m.to_string())
                .unwrap_or_else(|| "None".to_string()),
            indent = indent * 4
        )?;

        let mut children = Vec::new();

        let mut current_child = self.first_child.as_ref();
        while let Some(child) = current_child {
            children.push(child);
            current_child = child.next_sibling.as_ref();
        }

        children.sort_by(|a, b| b.number_of_visits.cmp(&a.number_of_visits));

        for child in children {
            child.fmt_with_indent(f, indent + 1)?;
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
            let mut best_ucb1 = f64::NEG_INFINITY;

            let mut current_child = (*current).first_child.as_deref_mut().unwrap() as *mut Node;
            let mut best_child = current_child; // TODO: Instead of best child, simulate best 'n' children

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

        for &ancestor in ancestors.iter().rev() {
            (*ancestor).total_score += rollout_score;
            (*ancestor).number_of_visits += 1;
        }
    }
}

fn rollout(current: &Node) -> i64 {
    let mut board = current.board;
    let mut rng = rand::thread_rng();
    let mut depth = 0;
    let max_depth = 100;

    loop {
        if depth >= max_depth || board.status() != BoardStatus::Ongoing {
            return evaluate_board(&board);
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
    let c = 1.414;
    exploitation + c * exploration_term
}

fn find_best_move(root: &Node) -> ChessMove {
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

    best_child.and_then(|child| child.move_to_reach).unwrap()
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
    let mut current_root: *mut Node = &mut root;

    loop {
        unsafe {
            dbg!(&*current_root);
        }
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        match parse(input) {
            Ok(("", UCICommand::UCI)) => {
                println!("id name {NAME} {VERSION}");
                println!("id author {AUTHORS}");
                println!("uciok");
            }
            Ok((_, UCICommand::IsReady)) => {
                println!("readyok");
            }
            Ok((_, UCICommand::Position { fen, moves })) => {
                if let Some(fen) = fen {
                    todo!("Implement setting position from FEN")
                }

                current_root = &mut root;

                for chess_move in moves {
                    unsafe {
                        // Check current_child nodes
                        let mut current_child = (*current_root).first_child.as_deref();
                        let move_to_make = uci_to_move(&chess_move).unwrap();

                        while let Some(child) = current_child {
                            if child.move_to_reach == Some(move_to_make) {
                                current_root = child as *const _ as *mut Node; // Update current_root
                                break;
                            }
                            current_child = child.next_sibling.as_deref();
                        }

                        // If the move is not found, create a new child node
                        if current_child.is_none() {
                            let new_board_state = (*current_root).board.make_move_new(move_to_make);
                            let new_node = Node::new(new_board_state, Some(move_to_make));
                            (*current_root).add_child(new_node.clone());

                            // Set current_root to the newly added child
                            current_root = (*current_root)
                                .first_child
                                .as_mut()
                                .expect("First child should not be None")
                                as &mut Node;
                        }
                    }
                }
            }
            #[rustfmt::skip]
            Ok((_, UCICommand::Go { wtime, btime, movestogo, movetime, depth, nodes })) => {
                let start_time = Instant::now();
                let time_limit = Duration::from_secs(1); // TODO: use adaptive timing

                unsafe {
                    while start_time.elapsed() < time_limit {
                        mcts(&mut *current_root);
                    }

                    let best_move = find_best_move(&*current_root);
                    println!("bestmove {}", best_move);
                }
            }
            Ok((_, UCICommand::Quit)) => break,
            _ => {}
        }
    }
}
