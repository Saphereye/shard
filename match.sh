#!/bin/bash

# Usage: ./run_match.sh /path/to/bot1 /path/to/bot2

BOT1="$1"
BOT2="$2"

if [[ ! -x "$BOT1" || ! -x "$BOT2" ]]; then
    echo "Error: One or both engine paths are not executable."
    echo "Usage: $0 /path/to/bot1 /path/to/bot2"
    exit 1
fi

cutechess-cli \
  -engine name=Bot1 cmd="$BOT1" \
  -engine name=Bot2 cmd="$BOT2" \
  -each proto=uci tc=40/60+0.1 \
  -rounds 10 \
  -concurrency 4 \
  -recover \
  -repeat \
  -games 10 \
  -pgnout results.pgn \
  -draw movenumber=50 movecount=8 score=10 \
  -resign movecount=3 score=800
