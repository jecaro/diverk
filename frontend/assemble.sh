#!/usr/bin/env bash
# Assemble frontend/dist/ from a pre-built wasm binary.
# Run from the frontend/ directory inside `nix develop`.
#
#   assemble.sh <wasm>          # dev: copy wasm as-is
#   assemble.sh <wasm> -Oz      # prod: wizer + wasm-opt -Oz + strip
set -euo pipefail

cd "$(dirname "$0")"

hs_wasm="$1"
shift

mkdir -p dist

"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
  --input "$hs_wasm" --output dist/ghc_wasm_jsffi.js

if [ $# -eq 0 ]; then
  cp "$hs_wasm" dist/bin.wasm
else
  env -i GHCRTS=-H64m "$(type -P wizer)" --allow-wasi --wasm-bulk-memory true \
    --inherit-env true --init-func _initialize -o dist/bin.wasm "$hs_wasm"
  wasm-opt "$@" dist/bin.wasm -o dist/bin.wasm
  wasm-tools strip -o dist/bin.wasm dist/bin.wasm
fi

cp index.html dist/

esbuild index.js --bundle --format=esm \
  --external:./ghc_wasm_jsffi.js --outfile=dist/index.js

if [ -d ../static/out ]; then
  # --no-preserve=mode because static/out may contain read-only files from the nix store
  cp -rL --no-preserve=mode ../static/out/css dist/css
  cp -rL --no-preserve=mode ../static/out/fontawesome dist/fontawesome
  echo "copied css/ and fontawesome/ from ../static/out"
else
  echo "WARNING: ../static/out not found; app will render unstyled."
  echo "  build CSS with: (cd ../static && ./build-css.sh)"
fi

echo "built dist/ ($(du -h dist/bin.wasm | cut -f1) wasm)"
