use crate::nnue::NNUE;
use chess::{Board, Color, Piece};

#[derive(Clone)]
pub struct Evaluator {
    nnue: NNUE,
}

impl Evaluator {
    /// Create evaluator, try to load NNUE model
    pub fn new() -> Self {
        let nnue = NNUE::load_from_numpy().unwrap();
        Self { nnue }
    }

    /// Main evaluation function for your engine
    /// Input: chess::Board  
    /// Output: evaluation in centipawns (+ = good for white)
    pub fn evaluate(&self, board: &Board) -> i32 {
             self.nnue.evaluate(board)    
    }
}
