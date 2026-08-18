import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

// The Reflex router redirects / → /repo via pushState on first load, which adds
// a spurious history entry and breaks webView.canGoBack(). Intercept that one
// push and convert it to replaceState so history starts clean at /repo.
{
  const origPush = history.pushState.bind(history);
  history.pushState = function(state, title, url) {
    history.pushState = origPush;
    if (window.location.pathname === '/') {
      history.replaceState(state, title, url);
    } else {
      origPush(state, title, url);
    }
  };
}

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ${msg}`)),
];

// jsaddle-wasm's synchronous-eval glue (embedded in ghc_wasm_jsffi.js) assigns
// to a bare `initialSyncDepth` global, which relied on sloppy-mode implicit
// globals. In an ES module (strict mode) that throws unless the name already
// resolves, so predeclare it on globalThis with its intended initial value.
globalThis.initialSyncDepth ??= 0;

const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("bin.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start(globalThis.example);
