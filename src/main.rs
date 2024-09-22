mod definitions;
mod utils;
use bitboards::BitBoard;
use definitions::*;
use utils::*;

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

    // let board = Board::default();
    // let board = Board::new("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
    // let board: Board = Board::new("rnbqkbnr/pp2pppp/2p5/3pP3/3P4/8/PPP2PPP/RNBQKBNR b KQkq - 0 1");
    let board = Board::new("rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1");
    println!("{}", board);
    for move_ in board.generate_moves() {
        println!("{}", move_);
    }
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
