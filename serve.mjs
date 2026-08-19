// Minimal static file server that sets the correct MIME types (notably
// `application/wasm`, which `WebAssembly.instantiateStreaming` requires and
// `python -m http.server` does not send).
//
//   node serve.mjs [DIR] [PORT]   # defaults: frontend/dist, 3000
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize } from "node:path";

const root = process.argv[2] ?? "frontend/dist";
const port = Number(process.argv[3] ?? 3000);

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

const GITHUB_PREFIX = "/api/github/";

createServer(async (req, res) => {
  // Proxy /api/github/* → https://api.github.com/* to avoid browser CORS restrictions.
  if (req.url.startsWith(GITHUB_PREFIX)) {
    const rest = req.url.slice(GITHUB_PREFIX.length);
    const target = "https://api.github.com/" + rest;
    try {
      const ghRes = await fetch(target, {
        method: req.method,
        headers: {
          ...(req.headers.authorization && { authorization: req.headers.authorization }),
          ...(req.headers.accept && { accept: req.headers.accept }),
          ...(req.headers["user-agent"] && { "user-agent": req.headers["user-agent"] }),
        },
      });
      res.statusCode = ghRes.status;
      res.setHeader("Content-Type", ghRes.headers.get("content-type") ?? "application/json");
      const buf = await ghRes.arrayBuffer();
      res.end(Buffer.from(buf));
    } catch (e) {
      res.statusCode = 502;
      res.end("proxy error: " + e.message);
    }
    return;
  }

  try {
    let path = decodeURIComponent(new URL(req.url, "http://x").pathname);
    if (path === "/") path = "/index.html";
    const file = join(root, normalize(path));
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
