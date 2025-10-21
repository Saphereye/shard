#![allow(static_mut_refs)]
use chess::Board;
use std::fs::File;
use timecat::nnue::HalfKPModel;
use timecat::nnue::HalfKPModelReader;
use timecat::BinRead;
use timecat::ChessPosition;

use std::sync::Once;

static mut MODEL: Option<Box<HalfKPModel>> = None;
static INIT: Once = Once::new();

pub fn evaluate_board(board: &Board) -> i16 {
    let position = ChessPosition::from_fen(&board.to_string()).unwrap();
    unsafe {
        INIT.call_once(|| {
            // Load NNUE file from assets directory
            let nnue_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("assets")
                .join("nn-62ef826d1a6d.nnue");
            
            let mut file = File::open(nnue_path).expect("Failed to open NNUE file");
            let reader = HalfKPModelReader::read(&mut file).expect("Failed to read NNUE model");
            MODEL = Some(Box::new(reader.to_default_model()));
        });
        
        MODEL.as_mut().unwrap().update_model_and_evaluate(&position)
    }
}
