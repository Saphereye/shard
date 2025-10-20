#![allow(static_mut_refs)]
// Modern evaluation using timecat's NNUE implementation
use chess::Board;
use std::io::Cursor;
use timecat::nnue::HalfKPModel;
use timecat::nnue::HalfKPModelReader;
use timecat::BinRead;
use timecat::ChessPosition;

use std::sync::Once;

static mut MODEL: Option<HalfKPModel> = None;
static INIT: Once = Once::new();

pub fn evaluate_board(board: &Board) -> i16 {
    let position = ChessPosition::from_fen(&board.to_string()).unwrap();
    unsafe {
        INIT.call_once(|| {
            // Try to load NNUE file - check multiple possible filenames
            let nnue_files = [
                "assets/nn-1c0000000000.nnue",
                "assets/nn-latest.nnue",
                "assets/nn-62ef826d1a6d.nnue",
                "nn-1c0000000000.nnue",
                "nn-latest.nnue",
            ];
            
            for filename in &nnue_files {
                if let Ok(data) = std::fs::read(filename) {
                    eprintln!("Loading NNUE from {}", filename);
                    let mut cursor = Cursor::new(&data[..]);
                    match HalfKPModelReader::read(&mut cursor) {
                        Ok(reader) => {
                            MODEL = Some(reader.to_default_model());
                            eprintln!("NNUE loaded successfully");
                            return;
                        }
                        Err(e) => {
                            eprintln!("Failed to parse {}: {:?}", filename, e);
                        }
                    }
                }
            }
            
            eprintln!("WARNING: No NNUE file found! Place nn-1c0000000000.nnue in assets/");
            eprintln!("Download from: https://tests.stockfishchess.org/nns");
            panic!("NNUE file required for evaluation");
        });
        
        MODEL.as_mut().unwrap().update_model_and_evaluate(&position)
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
