#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

npm run copy
cd android
gradle assembleDebug
echo "built android/build/android/app/outputs/apk/debug/app-debug.apk"
