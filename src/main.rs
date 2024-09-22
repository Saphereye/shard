mod definitions;
mod utils;
use bitboards::BitBoard;
use definitions::*;
use utils::*;

fn main() {
    println!("Hello, world!");
    let mut board = BitBoard::default();
    board.set(Square::D2);
    board.set(Square::D3);
    board.set(Square::D4);
    println!("{:?}", board);
    println!("{}", board.count());
    println!("{:?}", board.pop());
    println!("{}", board);

    let mut board = Board::default();
    println!("{}", board);
    // println!("{:0X}", Board::default().get_hash());
    // println!(
    //     "{:0X}",
    //     Board::new("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").get_hash()
    // );

    let mut board: Board =
        Board::new("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2");
    // board.update_piece_metadata();
    println!("{}", board);
    // println!("{:?}", board);
}
