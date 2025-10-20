// Modern evaluation supporting latest Stockfish NNUE format (HalfKAv2_hm)
use chess::Board;
use crate::nnue::{NNUE, classical_eval};
use std::sync::Once;

static mut MODEL: Option<NNUE> = None;
static INIT: Once = Once::new();

pub fn evaluate_board(board: &Board) -> i16 {
    unsafe {
        INIT.call_once(|| {
            // Try to load NNUE file - check multiple possible filenames
            let nnue_files = [
                "assets/nn-1c0000000000.nnue",
                "assets/nn-latest.nnue",
                "nn-1c0000000000.nnue",
                "nn-latest.nnue",
            ];
            
            for filename in &nnue_files {
                if let Ok(nnue) = NNUE::from_file(filename) {
                    eprintln!("NNUE loaded successfully from {}", filename);
                    MODEL = Some(nnue);
                    return;
                }
            }
            
            eprintln!("WARNING: No NNUE file found! Using classical evaluation.");
            eprintln!("Place nn-1c0000000000.nnue in assets/ directory");
            eprintln!("Download from: https://tests.stockfishchess.org/nns");
        });
        
        // Use NNUE if available, otherwise classical
        if let Some(ref nnue) = MODEL {
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
    
    // NNUE confidence based on position characteristics
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
