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

    let board = Board::default();
    println!("{}", board);
    // println!("{:0X}", Board::default().get_hash());
    // println!(
    //     "{:0X}",
    //     Board::new("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1").get_hash()
    // );

    let board: Board = Board::new("8/2P5/4B3/7p/1N1Q1p1P/ppK1Pk2/bR1r3P/8 w - - 0 1");
    dbg!(board.is_square_attacked(Square::D4));
    dbg!(board.is_square_attacked(Square::D5));
    dbg!(board.is_square_attacked(Square::C4));
    // board.update_piece_metadata();
    println!("{}", board);
    // println!("{:?}", board);
}
