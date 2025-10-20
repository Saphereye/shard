// Stockfish NNUE implementation for HalfKAv2_hm architecture
// Based on official Stockfish NNUE code

use chess::{Board, Color, Piece, Square, BitBoard};
use std::io::Read;

// Network dimensions
const TRANSFORMER_INPUTS: usize = 768;   // 12 pieces * 64 squares
const HIDDEN_SIZE: usize = 2048;          // L1 size
const L2_SIZE: usize = 16;                // L2 size  
const L3_SIZE: usize = 32;                // L3 size

// Quantization scales
const QA: i32 = 255;
const QB: i32 = 64;
const FV_SCALE: i32 = 16;

pub struct NNUE {
    // Feature transformer (768 -> 2048)
    ft_weights: Vec<i16>,  // [2048][768]
    ft_biases: Vec<i16>,   // [2048]
    
    // Layer 1 (2048*2 -> 16)
    l1_weights: Vec<i8>,   // [16][2048*2]
    l1_biases: Vec<i32>,   // [16]
    
    // Layer 2 (16 -> 32)
    l2_weights: Vec<i8>,   // [32][16]
    l2_biases: Vec<i32>,   // [32]
    
    // Layer 3 (32 -> 1)
    l3_weights: Vec<i8>,   // [1][32]
    l3_bias: i32,
}

impl NNUE {
    pub fn from_file(path: &str) -> Result<Self, String> {
        let data = std::fs::read(path)
            .map_err(|e| format!("Failed to read file: {}", e))?;
        Self::from_bytes(&data)
    }
    
    pub fn from_bytes(data: &[u8]) -> Result<Self, String> {
        let mut cursor = std::io::Cursor::new(data);
        
        // Read version (4 bytes)
        let mut version_bytes = [0u8; 4];
        cursor.read_exact(&mut version_bytes)
            .map_err(|e| format!("Failed to read version: {}", e))?;
        let version = u32::from_le_bytes(version_bytes);
        
        // Read hash (4 bytes)
        let mut hash_bytes = [0u8; 4];
        cursor.read_exact(&mut hash_bytes)
            .map_err(|e| format!("Failed to read hash: {}", e))?;
        let hash = u32::from_le_bytes(hash_bytes);
        
        // Read description size (4 bytes)
        let mut desc_size_bytes = [0u8; 4];
        cursor.read_exact(&mut desc_size_bytes)
            .map_err(|e| format!("Failed to read desc size: {}", e))?;
        let desc_size = u32::from_le_bytes(desc_size_bytes);
        
        // Skip description
        let mut desc = vec![0u8; desc_size as usize];
        cursor.read_exact(&mut desc)
            .map_err(|e| format!("Failed to read description: {}", e))?;
        
        // Read feature transformer - stored as [HIDDEN_SIZE][TRANSFORMER_INPUTS]
        let mut ft_biases = vec![0i16; HIDDEN_SIZE];
        for bias in ft_biases.iter_mut() {
            let mut bytes = [0u8; 2];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read FT bias: {}", e))?;
            *bias = i16::from_le_bytes(bytes);
        }
        
        // Weights stored as [HIDDEN_SIZE][TRANSFORMER_INPUTS]
        let mut ft_weights = vec![0i16; HIDDEN_SIZE * TRANSFORMER_INPUTS];
        for i in 0..HIDDEN_SIZE {
            for j in 0..TRANSFORMER_INPUTS {
                let mut bytes = [0u8; 2];
                cursor.read_exact(&mut bytes)
                    .map_err(|e| format!("Failed to read FT weight: {}", e))?;
                ft_weights[i * TRANSFORMER_INPUTS + j] = i16::from_le_bytes(bytes);
            }
        }
        
        // Read PSQT weights (8 buckets * 768 inputs) - skip for now as not used
        let psqt_size = 8 * TRANSFORMER_INPUTS;
        let mut _psqt_weights = vec![0i32; psqt_size];
        for weight in _psqt_weights.iter_mut() {
            let mut bytes = [0u8; 4];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read PSQT weight: {}", e))?;
            *weight = i32::from_le_bytes(bytes);
        }
        
        // Read L1 layer (2048*2 -> 16)
        let mut l1_biases = vec![0i32; L2_SIZE];
        for bias in l1_biases.iter_mut() {
            let mut bytes = [0u8; 4];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read L1 bias: {}", e))?;
            *bias = i32::from_le_bytes(bytes);
        }
        
        let mut l1_weights = vec![0i8; L2_SIZE * HIDDEN_SIZE * 2];
        for i in 0..L2_SIZE {
            for j in 0..(HIDDEN_SIZE * 2) {
                let mut byte = [0u8; 1];
                cursor.read_exact(&mut byte)
                    .map_err(|e| format!("Failed to read L1 weight: {}", e))?;
                l1_weights[i * (HIDDEN_SIZE * 2) + j] = byte[0] as i8;
            }
        }
        
        // Read L2 layer (16 -> 32)
        let mut l2_biases = vec![0i32; L3_SIZE];
        for bias in l2_biases.iter_mut() {
            let mut bytes = [0u8; 4];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read L2 bias: {}", e))?;
            *bias = i32::from_le_bytes(bytes);
        }
        
        let mut l2_weights = vec![0i8; L3_SIZE * L2_SIZE];
        for i in 0..L3_SIZE {
            for j in 0..L2_SIZE {
                let mut byte = [0u8; 1];
                cursor.read_exact(&mut byte)
                    .map_err(|e| format!("Failed to read L2 weight: {}", e))?;
                l2_weights[i * L2_SIZE + j] = byte[0] as i8;
            }
        }
        
        // Read L3 layer (32 -> 1)
        let mut l3_bias_bytes = [0u8; 4];
        cursor.read_exact(&mut l3_bias_bytes)
            .map_err(|e| format!("Failed to read L3 bias: {}", e))?;
        let l3_bias = i32::from_le_bytes(l3_bias_bytes);
        
        let mut l3_weights = vec![0i8; L3_SIZE];
        for weight in l3_weights.iter_mut() {
            let mut byte = [0u8; 1];
            cursor.read_exact(&mut byte)
                .map_err(|e| format!("Failed to read L3 weight: {}", e))?;
            *weight = byte[0] as i8;
        }
        
        eprintln!("NNUE loaded successfully (v:0x{:x} h:0x{:x})", version, hash);
        
        Ok(Self {
            ft_weights,
            ft_biases,
            l1_weights,
            l1_biases,
            l2_weights,
            l2_biases,
            l3_weights,
            l3_bias,
        })
    }
    
    fn get_feature_index(&self, piece: Piece, color: Color, square: Square, perspective: Color) -> usize {
        let piece_idx = match piece {
            Piece::Pawn => 0,
            Piece::Knight => 1,
            Piece::Bishop => 2,
            Piece::Rook => 3,
            Piece::Queen => 4,
            Piece::King => 5,
        };
        
        let color_offset = if color == perspective { 0 } else { 6 };
        let sq_idx = if perspective == Color::White {
            square.to_index()
        } else {
            square.to_index() ^ 56  // Flip vertically for black
        };
        
        (piece_idx + color_offset) * 64 + sq_idx
    }
    
    pub fn evaluate(&self, board: &Board) -> i16 {
        // Initialize accumulators with biases
        let mut acc_white = vec![0i16; HIDDEN_SIZE];
        let mut acc_black = vec![0i16; HIDDEN_SIZE];
        
        for i in 0..HIDDEN_SIZE {
            acc_white[i] = self.ft_biases[i];
            acc_black[i] = self.ft_biases[i];
        }
        
        // Add features for all pieces
        for color in [Color::White, Color::Black] {
            for piece in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen, Piece::King] {
                let pieces = board.pieces(piece) & board.color_combined(color);
                for square in pieces {
                    // White perspective
                    let idx_w = self.get_feature_index(piece, color, square, Color::White);
                    for i in 0..HIDDEN_SIZE {
                        // Weights are stored as [HIDDEN_SIZE][TRANSFORMER_INPUTS]
                        acc_white[i] += self.ft_weights[i * TRANSFORMER_INPUTS + idx_w];
                    }
                    
                    // Black perspective
                    let idx_b = self.get_feature_index(piece, color, square, Color::Black);
                    for i in 0..HIDDEN_SIZE {
                        acc_black[i] += self.ft_weights[i * TRANSFORMER_INPUTS + idx_b];
                    }
                }
            }
        }
        
        // Select perspective based on side to move
        let (us, them) = if board.side_to_move() == Color::White {
            (&acc_white, &acc_black)
        } else {
            (&acc_black, &acc_white)
        };
        
        // L1: Apply SqrClippedReLU (squared clipped ReLU) and forward
        let mut l1_out = vec![0i32; L2_SIZE];
        for i in 0..L2_SIZE {
            let mut sum = self.l1_biases[i];
            for j in 0..HIDDEN_SIZE {
                let us_val = us[j].clamp(0, QA as i16) as i32;
                let them_val = them[j].clamp(0, QA as i16) as i32;
                sum += us_val * self.l1_weights[i * (HIDDEN_SIZE * 2) + j] as i32;
                sum += them_val * self.l1_weights[i * (HIDDEN_SIZE * 2) + HIDDEN_SIZE + j] as i32;
            }
            // SqrClippedReLU: clamp then square
            let clamped = sum.clamp(0, QA * QB);
            l1_out[i] = clamped * clamped / (QA * QB);
        }
        
        // L2: Apply ClippedReLU and forward
        let mut l2_out = vec![0i32; L3_SIZE];
        for i in 0..L3_SIZE {
            let mut sum = self.l2_biases[i];
            for j in 0..L2_SIZE {
                sum += l1_out[j] * self.l2_weights[i * L2_SIZE + j] as i32;
            }
            l2_out[i] = sum.clamp(0, 127 * QB);
        }
        
        // L3: Output layer
        let mut output = self.l3_bias;
        for j in 0..L3_SIZE {
            output += l2_out[j] * self.l3_weights[j] as i32;
        }
        
        // Scale to centipawns (Stockfish uses 600 * 16 = 9600 as scale factor)
        (output / (QB * FV_SCALE)) as i16
    }
}

// Classical evaluation fallback
pub fn classical_eval(board: &Board) -> i16 {
    let mut score = 0i16;
    
    const PAWN_VALUE: i16 = 100;
    const KNIGHT_VALUE: i16 = 320;
    const BISHOP_VALUE: i16 = 330;
    const ROOK_VALUE: i16 = 500;
    const QUEEN_VALUE: i16 = 900;
    
    for color in [Color::White, Color::Black] {
        let sign = if color == Color::White { 1 } else { -1 };
        let pieces = board.color_combined(color);
        
        score += sign * (board.pieces(Piece::Pawn) & pieces).popcnt() as i16 * PAWN_VALUE;
        score += sign * (board.pieces(Piece::Knight) & pieces).popcnt() as i16 * KNIGHT_VALUE;
        score += sign * (board.pieces(Piece::Bishop) & pieces).popcnt() as i16 * BISHOP_VALUE;
        score += sign * (board.pieces(Piece::Rook) & pieces).popcnt() as i16 * ROOK_VALUE;
        score += sign * (board.pieces(Piece::Queen) & pieces).popcnt() as i16 * QUEEN_VALUE;
    }
    
    // Central control
    let white_pieces = board.color_combined(Color::White);
    let black_pieces = board.color_combined(Color::Black);
    let center_mask = BitBoard::from_square(Square::D4)
        | BitBoard::from_square(Square::D5)
        | BitBoard::from_square(Square::E4)
        | BitBoard::from_square(Square::E5);
    
    score += (white_pieces & center_mask).popcnt() as i16 * 10;
    score -= (black_pieces & center_mask).popcnt() as i16 * 10;
    
    if board.side_to_move() == Color::White {
        score
    } else {
        -score
    }
}
