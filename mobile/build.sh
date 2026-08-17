#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

npm run copy

cd android
if [ "${1:-}" = "release" ]; then
    gradle assembleRelease --no-daemon
elif [ "${1:-}" = "debug" ]; then
    gradle assembleDebug --no-daemon
else
    echo "Usage: build.sh [debug|release]" >&2
    exit 1
fi
