use rand::Rng;
use std::sync::LazyLock;

pub static ZOBRIST_TABLE: LazyLock<[[u64; 64]; 14]> = std::sync::LazyLock::new(|| {
    let mut rng = rand::thread_rng();
    let mut table = [[0u64; 64]; 14];

    for piece_table in &mut table {
        for square_hash in piece_table.iter_mut().take(64) {
            // Generate a random u64 value for each side
            *square_hash ^= rng.gen::<u64>();
        }
    }

    table
});
