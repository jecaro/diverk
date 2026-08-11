#!/usr/bin/env bash
# Build the Tailwind CSS + FontAwesome static assets into an output directory.
# Run inside `nix develop` (needs node/npm). Requires network on first run to
# populate node_modules.
#
#   ./build-css.sh [OUTDIR]   # default OUTDIR = static/out
#
# Produces:
#   OUTDIR/css/styles.css
#   OUTDIR/fontawesome/css/all.css
#   OUTDIR/fontawesome/webfonts/*
set -euo pipefail

cd "$(dirname "$0")/src"
out="${1:-$(cd .. && pwd)/out}"

mkdir -p "$out/css" "$out/fontawesome/css"

# Tailwind (base/components/utilities) + daisyui + autoprefixer + cssnano.
# tailwind.config.js scans ../../frontend/**/*.hs to purge unused classes.
npx postcss css/styles.css -o "$out/css/styles.css"

# FontAwesome, straight from its npm package.
fa=node_modules/@fortawesome/fontawesome-free
cp "$fa/css/all.min.css" "$out/fontawesome/css/all.css"
cp -r "$fa/webfonts" "$out/fontawesome/webfonts"

echo "css built into $out ($(du -h "$out/css/styles.css" | cut -f1) styles.css)"
