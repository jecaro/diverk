#!/usr/bin/env bash
# Build the Diverk frontend to WASM and assemble a runnable dist/.
# Run from the frontend/ directory inside `nix develop`.
#
#   ./build.sh            # dev build (fast, unoptimized ~large wasm)
#   ./build.sh -Oz        # prod build (wizer + wasm-opt <flags> + strip)
set -euo pipefail

cd "$(dirname "$0")"

wasm32-wasi-cabal build exe:frontend

hs_wasm=$(find ./dist-newstyle -name 'frontend.wasm' -print0 | xargs -0 ls -t 2>/dev/null | head -1)

bash assemble.sh "$hs_wasm" "$@"
