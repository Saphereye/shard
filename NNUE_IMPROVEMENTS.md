# NNUE-Guided Search Improvements

## Overview
This document describes the enhancements made to the Shard chess engine to better leverage NNUE (Neural Network Universal Evaluator) evaluations during search. These changes make the engine trust NNUE's positional understanding more and search NNUE-preferred moves more deeply, which should significantly improve playing strength.

## Problem Statement
The original implementation used NNUE only for leaf node evaluation in quiescence search. While this provided accurate position assessments, the search algorithm didn't use NNUE's insights to guide which moves to explore more deeply. This meant that promising moves identified by NNUE might be searched to the same depth as inferior alternatives, resulting in suboptimal play.

## Solution Overview
We integrated NNUE evaluations throughout the search process in three key areas:

1. **NNUE-Guided Move Ordering**: Prioritize moves that NNUE evaluates favorably
2. **Selective Search Extensions**: Search promising moves deeper
3. **NNUE-Based Pruning**: Skip unpromising moves earlier

## Detailed Changes

### 1. NNUE-Guided Move Ordering (`order_moves` function)

**What Changed:**
- Added NNUE evaluation to move scoring for quiet (non-tactical) moves
- Evaluates each position after making a move and uses the score in ordering
- Applied selectively (only at ply > 0 and for lower-priority moves) to balance accuracy with performance

**Why It Matters:**
- Better move ordering leads to more beta cutoffs (earlier pruning)
- Alpha-beta search is very sensitive to move order - searching the best move first can exponentially reduce the search tree
- NNUE's positional understanding helps identify strong quiet moves that traditional heuristics might miss

**Implementation Details:**
```rust
if ply > 0 && score > -5000 {
    let nnue_eval = evaluate_board(&new_board);
    let adjusted_eval = if side_to_move == Color::White {
        nnue_eval
    } else {
        -nnue_eval
    };
    // Weight NNUE evaluation in move ordering (scaled down to not dominate)
    score -= adjusted_eval / 20;
}
```

**Performance Impact:**
- Applied only to quiet moves (captures/promotions already prioritized)
- Only at ply > 0 (skips root node where all moves are searched anyway)
- Scaled by 1/20 to avoid dominating other important heuristics

### 2. Selective Search Extensions (`negamax` function)

**What Changed:**
- Extend search depth by 1 ply for moves that improve NNUE evaluation by >100 centipawns
- Also extend checks (already tactically important)

**Why It Matters:**
- Critical variations need deeper analysis
- NNUE can identify when a move significantly improves the position
- Extensions ensure we don't miss important tactics or positional gains due to horizon effects

**Implementation Details:**
```rust
if depth >= 2 && eval_improvement > 100 && new_board.checkers().popcnt() == 0 {
    extension = 1;
} else if new_board.checkers().popcnt() > 0 {
    extension = 1;
}
```

**Safety Conditions:**
- Only at depth >= 2 (prevents excessive extensions at leaves)
- Only for significant improvements (>100 cp)
- Still extends all checks regardless of NNUE eval

### 3. NNUE-Based Futility Pruning

**What Changed:**
- Skip late quiet moves that NNUE evaluates as significantly worse (>150 cp decline)
- Applied in shallow searches (depth <= 3) when position is not critical

**Why It Matters:**
- Saves search time by skipping moves that are clearly inferior
- More aggressive than traditional futility pruning because NNUE evaluation is more accurate
- Allows more time to be spent on promising variations

**Implementation Details:**
```rust
if move_count > 3 && depth <= 3 && 
   board.piece_on(chess_move.get_dest()).is_none() &&
   chess_move.get_promotion().is_none() &&
   new_board.checkers().popcnt() == 0 &&
   eval_improvement < -150 &&
   alpha > -CHECKMATE_SCORE + 1000 {
    
    self.position_history.pop_position();
    continue; // Skip this move
}
```

**Safety Conditions:**
- Only for late moves (move_count > 3)
- Only in shallow searches (depth <= 3)
- Not for captures, promotions, or checks
- Not in mating situations

### 4. NNUE-Aware Late Move Reductions

**What Changed:**
- Reduce search depth less (1 ply instead of 2) for moves that NNUE rates positively
- Standard reduction (2 ply) for moves NNUE doesn't favor

**Why It Matters:**
- LMR is a powerful technique but can cause problems if good moves are reduced too much
- NNUE helps identify which late moves still deserve reasonable search depth
- Balances search efficiency with accuracy

**Implementation Details:**
```rust
let reduction = if eval_improvement > 50 {
    1  // Smaller reduction for promising moves
} else if move_count > 8 {
    2
} else {
    1
};
```

## Expected Results

### Playing Strength
- **Expected ELO Gain**: 100-200 points
- Better tactical awareness in complex positions
- Improved positional play in quiet positions
- More accurate evaluation of candidate moves

### Search Efficiency
- Better move ordering → more beta cutoffs → smaller search tree
- More time spent on promising variations
- Less time wasted on obviously bad moves

### Characteristics
- More "human-like" play by trusting positional factors
- Better understanding of pawn structure, piece activity, and king safety
- Improved endgame technique

## Testing Recommendations

1. **Self-Play Testing**: Run games between the improved engine and the previous version
2. **Fixed-Depth Tests**: Compare node counts and move choices at fixed depths
3. **Time-Control Tests**: Play games with various time controls (blitz, rapid, classical)
4. **Position Tests**: Analyze performance on tactical test suites (e.g., Win at Chess)
5. **ELO Testing**: Play against known-strength opponents to measure improvement

## Configuration Parameters

The following thresholds can be tuned for optimal performance:

- **NNUE move ordering weight**: Currently `/20` (line ~775)
- **Extension threshold**: Currently `>100 cp` (line ~570)
- **Futility threshold**: Currently `<-150 cp` (line ~585)
- **LMR reduction threshold**: Currently `>50 cp` (line ~599)

## Potential Future Improvements

1. **Dynamic thresholds**: Adjust based on position type (tactical vs positional)
2. **Multi-PV integration**: Use NNUE to identify multiple promising lines
3. **Root move filtering**: Use NNUE to exclude obviously bad moves at root
4. **Aspiration windows**: Use NNUE to set better initial bounds
5. **Time management**: Allocate more time to positions where NNUE sees complexity

## Technical Notes

- All NNUE evaluations are cached in the transposition table
- NNUE evaluation adds computational cost, so it's applied selectively
- The implementation maintains the existing search infrastructure
- All changes are backward compatible with the UCI protocol

## Conclusion

These changes represent a significant evolution in how Shard uses NNUE. Rather than treating NNUE purely as a leaf evaluator, the engine now uses NNUE's insights throughout the search to guide exploration. This should result in stronger, more accurate play while maintaining good search efficiency.
