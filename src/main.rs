mod definitions;
mod utils;
use definitions::board::Board;
use definitions::castling::CastleType;
use definitions::move_::Move;
use definitions::piece::Piece;
use definitions::square::Square;

fn main() {
    // println!("Hello, world!");
    // let mut board = BitBoard::default();
    // board.set(Square::D2);
    // board.set(Square::D3);
    // board.set(Square::D4);
    // println!("{:?}", board);
    // println!("{}", board.count());
    // println!("{:?}", board.pop());
    // println!("{}", board);

    // let mut board = Board::default();
    // let mut board = Board::new("rnbqkbnr/ppp1p1pp/3p4/4Pp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3"); // En passant
    // let mut board = Board::new("6k1/p5pp/b4r2/5p2/1Pq1p3/1Qpp1PP1/P5BP/3RR1K1 b - - 0 30");
    let mut board = Board::new("rnbqkbnr/ppp1p1pp/8/4P3/2p2p2/5P2/PPPPN1PP/RNBQK2R w KQkq - 0 6"); // Castling
    println!("{}", board); // TODO fix castling permission based on moves, it store which rook has moved
    let mut castle_move = Move::new(
        Square::A1,
        Square::A1,
        Piece::None,
        false,
        false,
        Piece::None,
        CastleType::None,
        Piece::None,
    );
    for move_ in board.generate_moves() {
        if move_.castle != CastleType::None {
            castle_move = move_;
        }
        print!("{} ", move_);
    }
    println!();
    board.make_move(castle_move);
    println!("{}", board);
    board.undo_move();
    println!("{}", board);
    // println!("{:0X}", Board::default().get_hash());
    // println!(
    //     "{:0X}",
    //     Board::new("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").get_hash()
    // );

    // let board: Board = Board::new("rnbqkbnr/pp2pppp/2p5/3pP3/3P4/8/PPP2PPP/RNBQKBNR b KQkq - 0 1");
    // println!("{}", board);
    // dbg!(
    //     board.pieces[Square::D4 as usize],
    //     board.pieces[Square::D5 as usize],
    //     board.pieces[35],
    //     board.pieces[Square::D5.new_coor(-1, -1).unwrap()],
    //     board.pieces[Square::D5.new_coor(1, -1).unwrap()],
    //     board.pieces[Square::D5.new_coor(-1, -1).unwrap()],
    //     board.pieces[Square::D5.new_coor(1, 1).unwrap()],
    // );
    // // dbg!(board.is_square_attacked(Square::D4));
    // dbg!(board.is_square_attacked(Square::C4));
    // dbg!(board.is_square_attacked(Square::C5));
    // dbg!(board.piece_list[Piece::BP as usize]);
    // // board.update_piece_metadata();
    // // println!("{}", board);
    // for move_ in board.generate_moves() {
    //     println!("{}", move_);
    // }
    // println!("{:?}", board);
    //
    // let move_ = Move::new(
    //     Square::D2,
    //     Square::D4,
    //     Piece::BN,
    //     false,
    //     false,
    //     Piece::WR,
    //     false,
    // );
    // println!("{}", move_);
}
