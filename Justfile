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

clean:
    rm -rf dist-newstyle frontend/dist static/out
