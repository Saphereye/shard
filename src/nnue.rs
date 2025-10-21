// Stockfish NNUE implementation for HalfKAv2_hm architecture
// Based on official Stockfish NNUE code

use chess::{Board, Color, Piece, Square, BitBoard};
use std::io::Read;

// Network dimensions for HalfKAv2_hm
const PS_NB: usize = 11 * 64;  // 704
const TRANSFORMER_INPUTS: usize = 64 * PS_NB / 2;  // 22528
const L2_SIZE: usize = 16;                // Always 16 for this architecture (called 15 in SF but +1 for PSQT)
const L3_SIZE: usize = 32;                // Always 32

// Quantization scales
const QB: i32 = 64;  // 2^WeightScaleBits where WeightScaleBits = 6
const FV_SCALE: i32 = 16;  // OutputScale

pub struct NNUE {
    hidden_size: usize,    // Variable: 128, 2048, or 3072 depending on network
    
    // Feature transformer (TRANSFORMER_INPUTS -> hidden_size)
    ft_weights: Vec<i16>,  // [hidden_size][TRANSFORMER_INPUTS]
    ft_biases: Vec<i16>,   // [hidden_size]
    
    // Layer 1 (hidden_size*2 -> 16)
    l1_weights: Vec<i8>,   // [16][hidden_size*2]
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
        
        eprintln!("NNUE version: 0x{:x}", version);
        
        // Read hash (4 bytes)
        let mut hash_bytes = [0u8; 4];
        cursor.read_exact(&mut hash_bytes)
            .map_err(|e| format!("Failed to read hash: {}", e))?;
        let hash = u32::from_le_bytes(hash_bytes);
        
        eprintln!("NNUE hash: 0x{:x}", hash);
        
        // Read description size (4 bytes)
        let mut desc_size_bytes = [0u8; 4];
        cursor.read_exact(&mut desc_size_bytes)
            .map_err(|e| format!("Failed to read desc size: {}", e))?;
        let desc_size = u32::from_le_bytes(desc_size_bytes);
        
        // Read description to extract hidden size
        let mut desc = vec![0u8; desc_size as usize];
        cursor.read_exact(&mut desc)
            .map_err(|e| format!("Failed to read description: {}", e))?;
        
        // Parse architecture from description: "Features=HalfKAv2_hm(Friend)[41024->128x2]"
        // Extract the hidden size (128, 2048, or 3072)
        let desc_str = String::from_utf8_lossy(&desc);
        let hidden_size = if desc_str.contains("->128") {
            128
        } else if desc_str.contains("->2048") || desc_str.contains("->1536") {
            2048  // Some nets report 1536 but actually use 2048
        } else if desc_str.contains("->3072") {
            3072
        } else {
            // Default to 2048 if we can't parse
            eprintln!("Warning: Could not parse hidden size from description, defaulting to 2048");
            eprintln!("Description: {}", desc_str);
            2048
        };
        
        eprintln!("NNUE description: {} bytes", desc_size);
        eprintln!("NNUE loaded: FT={}x{}, L1={}x{}, L2={}x{}, L3={}x1",
                  TRANSFORMER_INPUTS, hidden_size, hidden_size*2, L2_SIZE, L2_SIZE, L3_SIZE, L3_SIZE);
        
        // Read feature transformer - stored as [hidden_size][TRANSFORMER_INPUTS]
        let mut ft_biases = vec![0i16; hidden_size];
        for bias in ft_biases.iter_mut() {
            let mut bytes = [0u8; 2];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read FT bias: {}", e))?;
            *bias = i16::from_le_bytes(bytes);
        }
        
        // Weights stored as [hidden_size][TRANSFORMER_INPUTS]
        let mut ft_weights = vec![0i16; hidden_size * TRANSFORMER_INPUTS];
        for i in 0..hidden_size {
            for j in 0..TRANSFORMER_INPUTS {
                let mut bytes = [0u8; 2];
                cursor.read_exact(&mut bytes)
                    .map_err(|e| format!("Failed to read FT weight: {}", e))?;
                ft_weights[i * TRANSFORMER_INPUTS + j] = i16::from_le_bytes(bytes);
            }
        }
        
        // Read PSQT weights (8 buckets * TRANSFORMER_INPUTS) - skip for now as not used
        let psqt_size = 8 * TRANSFORMER_INPUTS;
        let mut _psqt_weights = vec![0i32; psqt_size];
        for weight in _psqt_weights.iter_mut() {
            let mut bytes = [0u8; 4];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read PSQT weight: {}", e))?;
            *weight = i32::from_le_bytes(bytes);
        }
        
        // Read L1 layer (hidden_size*2 -> 16)
        let mut l1_biases = vec![0i32; L2_SIZE];
        for bias in l1_biases.iter_mut() {
            let mut bytes = [0u8; 4];
            cursor.read_exact(&mut bytes)
                .map_err(|e| format!("Failed to read L1 bias: {}", e))?;
            *bias = i32::from_le_bytes(bytes);
        }
        
        let mut l1_weights = vec![0i8; L2_SIZE * hidden_size * 2];
        for i in 0..L2_SIZE {
            for j in 0..(hidden_size * 2) {
                let mut byte = [0u8; 1];
                cursor.read_exact(&mut byte)
                    .map_err(|e| format!("Failed to read L1 weight: {}", e))?;
                l1_weights[i * (hidden_size * 2) + j] = byte[0] as i8;
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
        
        Ok(Self {
            hidden_size,
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
    
    // King bucket mapping for HalfKAv2_hm
    fn king_bucket(square: Square) -> usize {
        // Stockfish uses a complex bucket mapping, simplified here
        // Map squares to 4x8 = 32 buckets then mirror for black
        let sq = square.to_index();
        let file = sq % 8;
        let rank = sq / 8;
        
        // Map to buckets (simplified version)
        let bucket_file = file.min(7 - file);  // Mirror horizontally
        let bucket_rank = rank.min(7 - rank);  // Mirror vertically
        
        bucket_file / 2 + (bucket_rank / 2) * 4
    }
    
    fn get_feature_index(&self, piece: Piece, piece_color: Color, square: Square, 
                        king_sq: Square, perspective: Color) -> usize {
        // Skip if this is the king itself
        if piece == Piece::King {
            return 0; // Will be ignored
        }
        
        // Get king bucket
        let king_bucket = Self::king_bucket(king_sq);
        
        // Piece type index (0-9 for pawn through queen, friend and enemy)
        let piece_idx = match piece {
            Piece::Pawn => 0,
            Piece::Knight => 1,
            Piece::Bishop => 2,
            Piece::Rook => 3,
            Piece::Queen => 4,
            Piece::King => return 0, // Redundant check
        };
        
        // Add 5 if enemy piece
        let piece_offset = if piece_color == perspective {
            piece_idx
        } else {
            piece_idx + 5
        };
        
        // Orient square relative to perspective
        let oriented_sq = if perspective == Color::White {
            square.to_index()
        } else {
            square.to_index() ^ 56  // Vertical flip
        };
        
        // HalfKAv2_hm index: king_bucket * PS_NB + piece_offset * 64 + square
        king_bucket * PS_NB + piece_offset * 64 + oriented_sq
    }
    
    pub fn evaluate(&self, board: &Board) -> i16 {
        // Initialize accumulators with biases
        let mut acc_white = vec![0i32; self.hidden_size];
        let mut acc_black = vec![0i32; self.hidden_size];
        
        for i in 0..self.hidden_size {
            acc_white[i] = self.ft_biases[i] as i32;
            acc_black[i] = self.ft_biases[i] as i32;
        }
        
        // Get king squares
        let white_king_sq = (board.pieces(Piece::King) & board.color_combined(Color::White)).to_square();
        let black_king_sq = (board.pieces(Piece::King) & board.color_combined(Color::Black)).to_square();
        
        // Add features for all pieces
        for color in [Color::White, Color::Black] {
            for piece in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen, Piece::King] {
                let pieces = board.pieces(piece) & board.color_combined(color);
                for square in pieces {
                    // Skip kings as they don't have features
                    if piece == Piece::King {
                        continue;
                    }
                    
                    // White perspective
                    let idx_w = self.get_feature_index(piece, color, square, white_king_sq, Color::White);
                    for i in 0..self.hidden_size {
                        // Weights are stored as [hidden_size][TRANSFORMER_INPUTS]
                        acc_white[i] += self.ft_weights[i * TRANSFORMER_INPUTS + idx_w] as i32;
                    }
                    
                    // Black perspective
                    let idx_b = self.get_feature_index(piece, color, square, black_king_sq, Color::Black);
                    for i in 0..self.hidden_size {
                        acc_black[i] += self.ft_weights[i * TRANSFORMER_INPUTS + idx_b] as i32;
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
        // Formula: min(127, (input * input) >> (2 * WeightScaleBits + 7))
        // WeightScaleBits = 6, so shift by 19
        let mut l1_out = vec![0i8; L2_SIZE];
        for i in 0..L2_SIZE {
            let mut sum = self.l1_biases[i];
            for j in 0..self.hidden_size {
                // Clamp accumulator values to [0, 255] as per Stockfish
                let us_val = us[j].clamp(0, 255);
                let them_val = them[j].clamp(0, 255);
                sum += us_val * self.l1_weights[i * (self.hidden_size * 2) + j] as i32;
                sum += them_val * self.l1_weights[i * (self.hidden_size * 2) + self.hidden_size + j] as i32;
            }
            // SqrClippedReLU: square THEN shift and clamp
            // Input can be negative, squaring makes it positive
            let squared = (sum as i64) * (sum as i64);
            l1_out[i] = (squared >> 19).clamp(0, 127) as i8;
        }
        
        // L2: Apply ClippedReLU and forward
        // Output is divided by 2^WeightScaleBits (64) and clamped to [0, 127]
        let mut l2_out = vec![0i8; L3_SIZE];
        for i in 0..L3_SIZE {
            let mut sum = self.l2_biases[i];
            for j in 0..L2_SIZE {
                sum += l1_out[j] as i32 * self.l2_weights[i * L2_SIZE + j] as i32;
            }
            // ClippedReLU with shift by WeightScaleBits (6)
            l2_out[i] = (sum >> 6).clamp(0, 127) as i8;
        }
        
        // L3: Output layer
        let mut output = self.l3_bias;
        for j in 0..L3_SIZE {
            output += l2_out[j] as i32 * self.l3_weights[j] as i32;
        }
        
        // Scale to centipawns
        // Stockfish formula: output / (2^WeightScaleBits * OutputScale)
        // = output / (64 * 16) = output / 1024
        let eval = output / (QB * FV_SCALE);
        eval as i16
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
