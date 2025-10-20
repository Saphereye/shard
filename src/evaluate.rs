// Modern evaluation supporting both NNUE and classical evaluation
use chess::Board;
use crate::nnue::{NNUE, classical_eval};
use std::sync::Once;

static mut NNUE_MODEL: Option<NNUE> = None;
static INIT: Once = Once::new();

// Try to load NNUE, but gracefully fall back to classical eval if it fails
pub fn evaluate_board(board: &Board) -> i16 {
    unsafe {
        INIT.call_once(|| {
            // Try to load NNUE file if it exists
            if let Ok(data) = std::fs::read("assets/nn-latest.nnue")
                .or_else(|_| std::fs::read("assets/nn-62ef826d1a6d.nnue"))
            {
                if let Ok(nnue) = NNUE::from_bytes(&data) {
                    NNUE_MODEL = Some(nnue);
                    eprintln!("NNUE loaded successfully");
                } else {
                    eprintln!("Failed to parse NNUE file, using classical evaluation");
                }
            } else {
                eprintln!("No NNUE file found, using classical evaluation");
            }
        });
        
        // Use NNUE if available, otherwise use classical evaluation
        if let Some(ref nnue) = NNUE_MODEL {
            nnue.evaluate(board)
        } else {
            classical_eval(board)
        }
    }
}

// Get evaluation with confidence adjustment
// Returns (evaluation, confidence) where confidence is 0.0-1.0
pub fn evaluate_with_confidence(board: &Board) -> (i16, f32) {
    let eval = evaluate_board(board);
    
    // NNUE confidence is lower in the following situations:
    // 1. Very sharp positions (large material imbalance)
    // 2. Early game (opening positions)
    // 3. Endgame with few pieces
    
    let piece_count = board.combined().popcnt();
    let move_count = board.to_string().split_whitespace().nth(5)
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(0);
    
    // Base confidence
    let mut confidence = 1.0f32;
    
    // Reduce confidence in opening
    if move_count < 10 {
        confidence *= 0.7;
    }
    
    // Reduce confidence in endgame
    if piece_count < 10 {
        confidence *= 0.8;
    }
    
    // Reduce confidence for extreme evaluations (likely tactical)
    if eval.abs() > 500 {
        confidence *= 0.6;
    }
    
    (eval, confidence)
}
