// Minimal static file server that sets the correct MIME types (notably
// `application/wasm`, which `WebAssembly.instantiateStreaming` requires and
// `python -m http.server` does not send).
//
//   node serve.mjs [DIR] [PORT]   # defaults: frontend/dist, 8099
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize } from "node:path";

const root = process.argv[2] ?? "frontend/dist";
const port = Number(process.argv[3] ?? 8099);

const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".mjs": "text/javascript",
  ".wasm": "application/wasm",
  ".css": "text/css",
  ".json": "application/json",
  ".woff2": "font/woff2",
  ".woff": "font/woff",
  ".ttf": "font/ttf",
  ".svg": "image/svg+xml",
};

createServer(async (req, res) => {
  try {
    let path = decodeURIComponent(new URL(req.url, "http://x").pathname);
    if (path === "/") path = "/index.html";
    const file = join(root, normalize(path).replace(/^(\.\.[/\\])+/, ""));
    const body = await readFile(file);
    res.setHeader("Content-Type", types[extname(file)] ?? "application/octet-stream");
    res.end(body);
  } catch {
    try {
      const body = await readFile(join(root, "index.html"));
      res.setHeader("Content-Type", "text/html");
      res.end(body);
    } catch {
      res.statusCode = 404;
      res.end("not found");
    }
  }
}).listen(port, () => console.log(`serving ${root} on http://localhost:${port}`));
