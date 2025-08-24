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
            let data = std::fs::read("/home/adarsh/Downloads/nn-62ef826d1a6d.nnue").unwrap();
            let mut cursor = Cursor::new(data);
            let reader = HalfKPModelReader::read(&mut cursor).unwrap();
            MODEL = Some(reader.to_default_model());
        });
        
        MODEL.as_mut().unwrap().update_model_and_evaluate(&position)
    }
}
