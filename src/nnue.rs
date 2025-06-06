use chess::{Board, Color, Piece, Square, ALL_SQUARES};
use ndarray::{Array1, Array2};
use std::{collections::HashMap, fs::File};
use ndarray_npy::read_npy;

#[derive(Clone)]
pub struct NNUE {
    input_weights: Array2<f32>,
    input_bias: Array1<f32>,
    hidden_weights: Array2<f32>,
    hidden_bias: Array1<f32>,
    output_weights: Array2<f32>,
    output_bias: Array1<f32>,
}

impl NNUE {
    pub fn load_from_numpy() -> Result<Self, Box<dyn std::error::Error>> {
        let input_weights: Array2<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/input_layer.weight.npy")?;
        let input_bias: Array1<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/input_layer.bias.npy")?;
        let hidden_weights: Array2<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/hidden_layer.weight.npy")?;
        let hidden_bias: Array1<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/hidden_layer.bias.npy")?;
        let output_weights: Array2<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/output_layer.weight.npy")?;
        let output_bias: Array1<f32> = read_npy("/home/adarshdas1/Coding/shard/shard-trainer/nnue_export/output_layer.bias.npy")?;

        Ok(Self {
            input_weights,
            input_bias,
            hidden_weights,
            hidden_bias,
            output_weights,
            output_bias,
        })
    }

    pub fn evaluate(&self, board: &Board) -> i32 {
        let input = Array1::from_vec(encode_position(board));

        // Use .t() to transpose weight matrices (PyTorch stores them transposed)
let h1 = self.input_weights.dot(&input) + &self.input_bias;
let h1 = h1.mapv(|x| x.max(0.0)); // ReLU

let h2 = self.hidden_weights.dot(&h1) + &self.hidden_bias;
let h2 = h2.mapv(|x| x.max(0.0)); // ReLU

let out = self.output_weights.dot(&h2) + &self.output_bias;
        (out[0] * 100.0).round() as i32
    }
}

fn encode_position(board: &Board) -> Vec<f32> {
    let mut encoding = vec![0.0; 12 * 64];

    let piece_map: HashMap<(Piece, Color), usize> = [
        (Piece::Pawn, Color::White, 0),
        (Piece::Knight, Color::White, 1),
        (Piece::Bishop, Color::White, 2),
        (Piece::Rook, Color::White, 3),
        (Piece::Queen, Color::White, 4),
        (Piece::King, Color::White, 5),
        (Piece::Pawn, Color::Black, 6),
        (Piece::Knight, Color::Black, 7),
        (Piece::Bishop, Color::Black, 8),
        (Piece::Rook, Color::Black, 9),
        (Piece::Queen, Color::Black, 10),
        (Piece::King, Color::Black, 11),
    ]
    .iter()
    .cloned()
    .map(|(p, c, i)| ((p, c), i))
    .collect();

    for square in ALL_SQUARES {
        if let Some(piece) = board.piece_on(square) {
            if let Some(color) = board.color_on(square) {
                if let Some(&index) = piece_map.get(&(piece, color)) {
                    let sq_idx = square.to_index();
                    encoding[index * 64 + sq_idx] = 1.0;
                }
            }
        }
    }

    encoding
}
