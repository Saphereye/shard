#![allow(static_mut_refs)]
use chess::Board;
use timecat::evaluate::Evaluator;
use timecat::ChessPosition;
use timecat::utils::extension_traits::PositionEvaluation;

use std::sync::Once;

static mut EVALUATOR: Option<Evaluator> = None;
static INIT: Once = Once::new();

pub fn evaluate_board(board: &Board) -> i16 {
    let position = ChessPosition::from_fen(&board.to_string()).unwrap();
    unsafe {
        INIT.call_once(|| {
            // Use timecat's Default evaluator which includes built-in NNUE when inbuilt_nnue feature is enabled
            EVALUATOR = Some(Evaluator::default());
        });
        
        EVALUATOR.as_mut().unwrap().evaluate(&position) as i16
    }
}
