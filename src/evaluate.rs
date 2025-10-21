#![allow(static_mut_refs)]
use chess::Board;
use std::fs::File;
use std::sync::Once;
use timecat::nnue::HalfKPModel;
use timecat::nnue::HalfKPModelReader;
use timecat::BinRead;
use timecat::ChessPosition;

static mut MODEL: Option<Box<HalfKPModel>> = None;
static INIT: Once = Once::new();

/// Initialize the NNUE model. Should be called at engine startup.
pub fn init_nnue() {
    unsafe {
        INIT.call_once(|| {
            let nnue_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("assets")
                .join("nn-62ef826d1a6d.nnue");
            
            let mut file = File::open(nnue_path).expect("Failed to open NNUE file");
            let reader = HalfKPModelReader::read(&mut file).expect("Failed to read NNUE model");
            MODEL = Some(Box::new(reader.to_default_model()));
        });
    }
}

pub fn evaluate_board(board: &Board) -> i16 {
    let position = ChessPosition::from_fen(&board.to_string()).unwrap();
    unsafe {
        // Ensure model is initialized
        init_nnue();
        MODEL.as_mut().unwrap().update_model_and_evaluate(&position)
    }
}
