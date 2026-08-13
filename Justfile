default: all

css:
    cd static && ./build-css.sh

wasm:
    cd frontend && ./build.sh

wasm-prod:
    cd frontend && ./build.sh -Oz

all: css wasm

prod: css wasm-prod

run: all
    node serve.mjs

android: all
    cd mobile && ./build.sh

clean:
    # cabal build artifacts
    rm -rf dist-newstyle
    # assembled web app (wasm + bundled js + html + css)
    rm -rf frontend/dist
    # compiled css
    rm -rf static/out
    # gradle build output (apk + intermediates)
    rm -rf mobile/android/build
    # capacitor generated android plugin scaffolding
    rm -rf mobile/android/capacitor-cordova-android-plugins
    # web assets copied into android project by cap sync
    rm -rf mobile/android/app/src/main/assets
