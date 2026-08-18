#!/usr/bin/env bash
# Build the Diverk frontend to WASM and assemble a runnable dist/.
# Run from the frontend/ directory inside `nix develop`.
#
#   ./build.sh            # dev build (fast, unoptimized ~large wasm)
#   ./build.sh -Oz        # prod build (wizer + wasm-opt <flags> + strip)
#
# Static assets (css/, fontawesome/) are copied from ../static-result if that
# symlink exists (created by: nix build .#static -o static-result), otherwise
# skipped with a warning.
set -euo pipefail

cd "$(dirname "$0")"

if [ $# -eq 0 ]; then
  echo "Building for dev"
  dev_mode=true
else
  echo "Building for prod"
  dev_mode=false
fi

wasm32-wasi-cabal build exe:frontend

hs_wasm=$(find ../dist-newstyle -name 'frontend.wasm' -print0 | xargs -0 ls -t 2>/dev/null | head -1)

mkdir -p dist

"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
  --input "$hs_wasm" --output dist/ghc_wasm_jsffi.js

if $dev_mode; then
  cp "$hs_wasm" dist/bin.wasm
else
  env -i GHCRTS=-H64m "$(type -P wizer)" --allow-wasi --wasm-bulk-memory true \
    --inherit-env true --init-func _initialize -o dist/bin.wasm "$hs_wasm"
  wasm-opt "$@" dist/bin.wasm -o dist/bin.wasm
  wasm-tools strip -o dist/bin.wasm dist/bin.wasm
fi

cp index.html dist/

# Bundle npm modules from index.js
esbuild index.js --bundle --format=esm \
  --external:./ghc_wasm_jsffi.js --outfile=dist/index.js

# Static assets (Tailwind CSS + FontAwesome), if built.
if [ -d ../static/out ]; then
  # --no-preserve=mode because static/out may contain read-only files copied from the nix store
  cp -rL --no-preserve=mode ../static/out/css dist/css
  cp -rL --no-preserve=mode ../static/out/fontawesome dist/fontawesome
  echo "copied css/ and fontawesome/ from ../static/out"
else
  echo "WARNING: ../static/out not found; app will render unstyled."
  echo "  build CSS with: (cd ../static && ./build-css.sh)"
fi

echo "built dist/ ($(du -h dist/bin.wasm | cut -f1) wasm)"
