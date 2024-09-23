#[rustfmt::skip]
pub enum Castling {
    WK = 0b0001,
    WQ = 0b0010,
    BK = 0b0100,
    BQ = 0b1000,
}

#[derive(Debug)]
pub enum CastleType {
    KingSide,
    QueenSide,
    None,
}
