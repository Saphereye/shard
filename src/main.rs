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
    println!("{}", board);
    println!("{}", board.count());
    println!("{:?}", board.pop());
    println!("{}", board);

    let mut board = Board::default();
    println!("{}", board);
    println!("{}", Board::default().get_hash());
    println!("{}", Board::new().get_hash());
}
