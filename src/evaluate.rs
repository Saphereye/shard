#![allow(static_mut_refs)]
use chess::Board;
use std::hash::Hash;
use std::io::Cursor;
use timecat::nnue::HalfKPModel;
use timecat::nnue::HalfKPModelReader;
use timecat::BinRead;
use timecat::ChessPosition;

use std::sync::Once;

#[derive(Clone)]
struct StaticEvaluationEntry {
    key: u32,          // Upper 32 bits of hash for verification
    score: i16,
}

static mut MODEL: Option<HalfKPModel> = None;
static INIT: Once = Once::new();
static mut STATIC_EVALUATION_TABLE: Vec<Option<StaticEvaluationEntry>> = Vec::new();
const EVAL_TABLE_SIZE: usize = 1 << 20;

pub fn evaluate_board(board: &Board) -> i16 {
    let position = ChessPosition::from_fen(&board.to_string()).unwrap();
    let hash = board.get_hash();
    let table_index = (hash % (1 << 20)) as usize;

    unsafe {
        INIT.call_once(|| {
            let data = include_bytes!("../assets/nn-62ef826d1a6d.nnue");
            let mut cursor = Cursor::new(data);
            let reader = HalfKPModelReader::read(&mut cursor).unwrap();
            MODEL = Some(reader.to_default_model());
            STATIC_EVALUATION_TABLE = vec![None; EVAL_TABLE_SIZE];
        });

        if let Some(ref entry) = STATIC_EVALUATION_TABLE[table_index] {
            if entry.key == (hash >> 32) as u32 {
                return entry.score;
            }
        }

        let evaluation = MODEL.as_mut().unwrap().update_model_and_evaluate(&position);

        STATIC_EVALUATION_TABLE[table_index] = Some(StaticEvaluationEntry { key: (hash >> 32) as u32, score: evaluation });

        evaluation
    }
}
