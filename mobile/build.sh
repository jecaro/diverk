#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

npm run copy

cd android
if [ "${1:-}" = "release" ]; then
    gradle assembleRelease --no-daemon
else
    gradle assembleDebug --no-daemon
fi
