mod definitions;
mod utils;
use clap::Parser;
use definitions::board::Board;
use definitions::castling::CastleType;
use definitions::move_::Move;
use definitions::piece::Piece;
use definitions::square::Square;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    fen: String,
}

fn main() {
    let args = Args::parse();
    // Fixing:
    // - en passant capture checks opponent:
    // - castling (including losing cr due to rook capture):
    // - castling prevented

    // let mut board = Board::default();
    // let mut board = Board::new("rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"); // En passant
    // let mut board = Board::new("6k1/p5pp/b4r2/5p2/1Pq1p3/1Qpp1PP1/P5BP/3RR1K1 b - - 0 30");
    // let mut board = Board::new("rnbqkbnr/ppp1p1pp/8/4P3/2p2p2/5P2/PPPPN1PP/RNBQK2R w KQkq - 0 6"); // Castling
    // let mut board = Board::new("8/P3k3/8/8/4p3/8/3K4/8 w - - 0 1"); // Endgame
    // let mut board = Board::new("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ");
    // let mut board = Board::new("8/8/1k6/8/2pP4/8/5BK1/8 b - d3 0 1");
    let mut board = Board::new(args.fen);
    // println!("{}", board);
    let mut count = 0;
    let mut moves = String::new();
    for move_ in board.get_legal_moves() {
        board.make_move(move_);
        // println!("{}\n{}", move_, board);
        moves.push_str(&format!("{} ", move_));
        board.undo_move();
        count += 1;
    }
    moves.push_str("\n");
    // println!("{}", count);
    println!("{}", moves);

    // println!("{}", board.perft_test(6));
}
