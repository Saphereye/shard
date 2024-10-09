#!/bin/bash
RUSTFLAGS="-C target-cpu=native" cargo build --release
cp ./target/release/shard /home/adarsh/Coding/lichess-bot/engines/shard
