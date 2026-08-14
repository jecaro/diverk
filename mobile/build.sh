#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

npm run copy

cd android
gradle assembleDebug --no-daemon
