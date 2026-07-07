{
  description = "Diverk — GHC WebAssembly (Reflex) dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    ghc-wasm-meta = {
      url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
    };
  };

  outputs =
    { self, nixpkgs, ghc-wasm-meta }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      # Bundles wasm32-wasi-ghc, wasm32-wasi-cabal, wasm32-wasi-ghc-pkg,
      # wasm32-wasi-hsc2hs, wizer, wasm-opt, wasm-tools and post-link.mjs
      # (via `wasm32-wasi-ghc --print-libdir`).
      #
      # Pinned to GHC 9.12: reflex-dom-core caps base < 4.22, so GHC 9.14
      # (base 4.22) does not resolve. 9.12 (base 4.21) is the version the
      # upstream ghc-wasm-reflex-examples validates against.
      wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_12;
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "diverk-wasm";
        packages = [
          wasmToolchain
          # Native Haskell tooling for editor/tooling use.
          pkgs.cabal-install
          # JS/CSS toolchain: WASI shim deps, tailwind/postcss build, http-server.
          pkgs.nodejs
          pkgs.git
        ];

        shellHook = ''
          echo "diverk wasm dev shell"
          echo "  wasm32-wasi-ghc:   $(command -v wasm32-wasi-ghc || echo 'MISSING')"
          echo "  wasm32-wasi-cabal: $(command -v wasm32-wasi-cabal || echo 'MISSING')"
          echo "  node:              $(command -v node || echo 'MISSING')"
        '';
      };
    };
}
