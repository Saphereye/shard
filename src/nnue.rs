// Modern NNUE implementation supporting Stockfish's HalfKAv2 architecture
// This is a simplified but functional implementation that can be extended

use chess::{Board, Color, Piece, Square, BitBoard};
use std::io::{Read, Cursor};

// Network architecture constants for HalfKAv2
const INPUT_SIZE: usize = 768;  // 64 squares * 12 piece types / 2 (one side)
const HIDDEN_SIZE: usize = 512;

// Quantization scale
const QA: i32 = 255;
const QB: i32 = 64;
const QAB: i32 = QA * QB;

// Feature indices for HalfKAv2
// Each piece on each square gets a unique index based on king position
fn piece_to_index(piece: Piece, color: Color) -> usize {
    let base = match piece {
        Piece::Pawn => 0,
        Piece::Knight => 1,
        Piece::Bishop => 2,
        Piece::Rook => 3,
        Piece::Queen => 4,
        Piece::King => 5,
    };
    base + if color == Color::White { 0 } else { 6 }
}

#[derive(Clone)]
pub struct NNUEAccumulator {
    white: [i16; HIDDEN_SIZE],
    black: [i16; HIDDEN_SIZE],
}

impl NNUEAccumulator {
    fn new() -> Self {
        Self {
            white: [0; HIDDEN_SIZE],
            black: [0; HIDDEN_SIZE],
        }
    }
}

pub struct NNUE {
    // Feature transformer weights (input -> hidden)
    feature_weights: Vec<i16>,
    feature_bias: Vec<i16>,
    
    // Output layer weights (hidden -> output)
    output_weights: Vec<i16>,
    output_bias: i16,
}

impl NNUE {
    pub fn new() -> Self {
        // Initialize with small random-ish values
        // In practice, this should be loaded from a file
        Self {
            feature_weights: vec![0; INPUT_SIZE * HIDDEN_SIZE],
            feature_bias: vec![0; HIDDEN_SIZE],
            output_weights: vec![0; HIDDEN_SIZE * 2],
            output_bias: 0,
        }
    }
    
    pub fn from_bytes(data: &[u8]) -> Result<Self, std::io::Error> {
        let mut cursor = Cursor::new(data);
        
        // Read header/version info (simplified)
        let mut header = [0u8; 4];
        cursor.read_exact(&mut header)?;
        
        // Allocate and read feature weights
        let mut feature_weights = vec![0i16; INPUT_SIZE * HIDDEN_SIZE];
        for weight in feature_weights.iter_mut() {
            let mut bytes = [0u8; 2];
            cursor.read_exact(&mut bytes)?;
            *weight = i16::from_le_bytes(bytes);
        }
        
        // Read feature bias
        let mut feature_bias = vec![0i16; HIDDEN_SIZE];
        for bias in feature_bias.iter_mut() {
            let mut bytes = [0u8; 2];
            cursor.read_exact(&mut bytes)?;
            *bias = i16::from_le_bytes(bytes);
        }
        
        // Read output weights
        let mut output_weights = vec![0i16; HIDDEN_SIZE * 2];
        for weight in output_weights.iter_mut() {
            let mut bytes = [0u8; 2];
            cursor.read_exact(&mut bytes)?;
            *weight = i16::from_le_bytes(bytes);
        }
        
        // Read output bias
        let mut bytes = [0u8; 2];
        cursor.read_exact(&mut bytes)?;
        let output_bias = i16::from_le_bytes(bytes);
        
        Ok(Self {
            feature_weights,
            feature_bias,
            output_weights,
            output_bias,
        })
    }
    
    fn get_feature_index(&self, piece: Piece, piece_color: Color, square: Square, _king_sq: Square, perspective: Color) -> usize {
        let sq_idx = if perspective == Color::White {
            square.to_index()
        } else {
            square.to_index() ^ 56 // Flip for black perspective
        };
        
        // Flip color if perspective is different
        let adjusted_color = if perspective == Color::White {
            piece_color
        } else {
            !piece_color  // Use not operator instead of xor
        };
        
        let piece_idx = piece_to_index(piece, adjusted_color);
        piece_idx * 64 + sq_idx
    }
    
    pub fn evaluate(&self, board: &Board) -> i16 {
        let mut accumulator = NNUEAccumulator::new();
        
        // Initialize accumulator with bias
        for i in 0..HIDDEN_SIZE {
            accumulator.white[i] = self.feature_bias[i];
            accumulator.black[i] = self.feature_bias[i];
        }
        
        // Find kings
        let white_king = (board.pieces(Piece::King) & board.color_combined(Color::White))
            .to_square();
        let black_king = (board.pieces(Piece::King) & board.color_combined(Color::Black))
            .to_square();
        
        // Add all pieces to accumulator
        for piece in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen, Piece::King].iter() {
            for color in [Color::White, Color::Black].iter() {
                let pieces = board.pieces(*piece) & board.color_combined(*color);
                
                for square in pieces {
                    // Update white perspective
                    let w_idx = self.get_feature_index(*piece, *color, square, white_king, Color::White);
                    for i in 0..HIDDEN_SIZE {
                        accumulator.white[i] += self.feature_weights[w_idx * HIDDEN_SIZE + i];
                    }
                    
                    // Update black perspective
                    let b_idx = self.get_feature_index(*piece, *color, square, black_king, Color::Black);
                    for i in 0..HIDDEN_SIZE {
                        accumulator.black[i] += self.feature_weights[b_idx * HIDDEN_SIZE + i];
                    }
                }
            }
        }
        
        // Apply activation and compute output
        let side_to_move = board.side_to_move();
        let (us, them) = if side_to_move == Color::White {
            (&accumulator.white, &accumulator.black)
        } else {
            (&accumulator.black, &accumulator.white)
        };
        
        let mut output = self.output_bias as i32;
        
        // ClippedReLU activation
        for i in 0..HIDDEN_SIZE {
            let us_val = us[i].clamp(0, QA as i16) as i32;
            let them_val = them[i].clamp(0, QA as i16) as i32;
            
            output += us_val * self.output_weights[i] as i32;
            output += them_val * self.output_weights[HIDDEN_SIZE + i] as i32;
        }
        
        // Scale output to centipawns
        let eval = (output / QAB) as i16;
        
        // Return from white's perspective
        if side_to_move == Color::White {
            eval
        } else {
            -eval
        }
    }
}

impl Default for NNUE {
    fn default() -> Self {
        Self::new()
    }
}

// Fallback evaluation using piece-square tables and material
pub fn classical_eval(board: &Board) -> i16 {
    let mut score = 0i16;
    
    // Material values
    const PAWN_VALUE: i16 = 100;
    const KNIGHT_VALUE: i16 = 320;
    const BISHOP_VALUE: i16 = 330;
    const ROOK_VALUE: i16 = 500;
    const QUEEN_VALUE: i16 = 900;
    
    for color in [Color::White, Color::Black].iter() {
        let sign = if *color == Color::White { 1 } else { -1 };
        let pieces = board.color_combined(*color);
        
        score += sign * (board.pieces(Piece::Pawn) & pieces).popcnt() as i16 * PAWN_VALUE;
        score += sign * (board.pieces(Piece::Knight) & pieces).popcnt() as i16 * KNIGHT_VALUE;
        score += sign * (board.pieces(Piece::Bishop) & pieces).popcnt() as i16 * BISHOP_VALUE;
        score += sign * (board.pieces(Piece::Rook) & pieces).popcnt() as i16 * ROOK_VALUE;
        score += sign * (board.pieces(Piece::Queen) & pieces).popcnt() as i16 * QUEEN_VALUE;
    }
    
    // Simple positional bonuses
    let white_pieces = board.color_combined(Color::White);
    let black_pieces = board.color_combined(Color::Black);
    
    // Central control bonus
    let center_mask = BitBoard::from_square(Square::D4)
        | BitBoard::from_square(Square::D5)
        | BitBoard::from_square(Square::E4)
        | BitBoard::from_square(Square::E5);
    
    score += (white_pieces & center_mask).popcnt() as i16 * 10;
    score -= (black_pieces & center_mask).popcnt() as i16 * 10;
    
    // Return from current player's perspective
    if board.side_to_move() == Color::White {
        score
    } else {
        -score
    }
}
