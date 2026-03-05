#!/bin/bash
set -e

cargo build --release

# Extract version from Cargo.toml
VERSION=$(grep '^version' Cargo.toml | head -1 | sed 's/version = "//;s/"//')

DEST="./versions/shard_${VERSION}"

cp target/release/shard "$DEST"

echo "Built and copied to $DEST"
