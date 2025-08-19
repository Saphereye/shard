#!/bin/bash
# Usage: ./run_match_elo.sh /path/to/your_engine [target_elo]

TARGET_ENGINE="$1"
TARGET_ELO="${2:-1800}"  # Default to 1800 Elo if not specified

if [[ ! -x "$TARGET_ENGINE" ]]; then
    echo "Error: Engine path is not executable."
    echo "Usage: $0 /path/to/your_engine [target_elo]"
    echo "Target Elo range: 1320-3190 (Stockfish's UCI_Elo limits)"
    exit 1
fi

# Check if stockfish is available in PATH
if ! command -v stockfish &> /dev/null; then
    echo "Error: 'stockfish' command not found in PATH."
    echo "Make sure Stockfish is installed and accessible."
    exit 1
fi

# Validate Elo range
if [[ $TARGET_ELO -lt 1320 || $TARGET_ELO -gt 3190 ]]; then
    echo "Error: Target Elo must be between 1320 and 3190"
    exit 1
fi

echo "Testing your engine against Stockfish limited to $TARGET_ELO Elo"
echo "Note: This Elo is calibrated at 120s+1s time control and anchored to CCRL 40/4"
echo ""

# Suggest appropriate Elo levels for testing
echo "Suggested testing progression:"
echo "  1400 Elo - Beginner level"
echo "  1600 Elo - Club player level"
echo "  1800 Elo - Strong club player"
echo "  2000 Elo - Expert level"
echo "  2200 Elo - Master level"
echo "  2400 Elo - International Master level"
echo "  2600 Elo - Grandmaster level"
echo "  2800+ Elo - Super-GM/Engine level"
echo ""

cutechess-cli \
  -engine name="$TARGET_ENGINE" cmd="$TARGET_ENGINE" \
  -engine name="Stockfish-${TARGET_ELO}" cmd="stockfish" option.UCI_LimitStrength=true option.UCI_Elo=$TARGET_ELO \
  -each proto=uci tc=40/60+0.1 \
  -rounds 5 \
  -concurrency 4 \
  -recover \
  -repeat \
  -games 5 \
  -pgnout "results_vs_stockfish_${TARGET_ELO}elo.pgn" \
  -draw movenumber=50 movecount=8 score=10 \
  -resign movecount=3 score=800

echo ""
echo "Results saved to: results_vs_stockfish_${TARGET_ELO}elo.pgn"
echo ""
echo "To test different Elo levels, run:"
echo "./run_match_elo.sh $TARGET_ENGINE 1400  # Beginner level"
echo "./run_match_elo.sh $TARGET_ENGINE 1800  # Strong club player"
echo "./run_match_elo.sh $TARGET_ENGINE 2200  # Master level"
echo "./run_match_elo.sh $TARGET_ENGINE 2600  # Grandmaster level"
echo ""
echo "Tip: If you score ~50% against an Elo level, your engine is approximately that strength!"
