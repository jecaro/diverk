{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-wasm.url = "github:ners/nix-wasm";

    obelisk = {
      url = "github:obsidiansystems/obelisk/b0b16ee72b90517d0ee90e5ea670ed76345dde57";
      flake = false;
    };
  };

  outputs =
    { self, nixpkgs, nix-wasm, obelisk }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        # Required to compose the (unfree) Android SDK for the `android` shell.
        config.allowUnfree = true;
        config.android_sdk.accept_license = true;
      };

      # wasm32-wasi-ghc, wasm32-wasi-cabal, wizer, wasm-opt, post-link.mjs, …
      wasmToolchain = nix-wasm.inputs.ghc-wasm-meta.packages.${system}.all_9_14;

      # nix-wasm's legacyPackages is a patched nixpkgs that cross-compiles
      # Haskell to wasm32-wasi.  Use only its own fetchFromGitHub / haskell.lib
      # so that the WASM package set evaluation stays entirely within nix-wasm's
      # pinned nixpkgs — mixing in our (newer) nixpkgs would cause different
      # package versions to be selected and break jailbreak-cabal's constraint
      # lifting for those packages.
      wasmPkgs = nix-wasm.legacyPackages.${system};

      obeliskSrc = obelisk;

      wasmHaskellPkgs = wasmPkgs.haskell.packages.ghc914.extend (
        hfinal: hprev: {
          obelisk-route = hfinal.callCabal2nix "obelisk-route" "${obeliskSrc}/lib/route" { };

          tabulation = hfinal.callCabal2nix "tabulation" "${obeliskSrc}/lib/tabulation" { };

          obelisk-executable-config-lookup = hfinal.callCabal2nix "obelisk-executable-config-lookup"
            "${obeliskSrc}/lib/executable-config/lookup"
            { };

          # parser-regex-0.3.0.0 bounds ghc-bignum < 1.4 but GHC 9.14 ships 1.4.
          # jsaddle-0.9.9.3 bounds time < 1.15 but GHC 9.14 ships 1.15.
          # jailbreak-cabal can't strip these because they're inside conditional
          # `if impl(ghc ...)` blocks, so we patch them directly.
          parser-regex = wasmPkgs.haskell.lib.overrideCabal hprev.parser-regex (old: {
            prePatch = (old.prePatch or "") + ''
              substituteInPlace parser-regex.cabal \
                --replace-fail 'ghc-bignum >= 1.1 && < 1.4' 'ghc-bignum >= 1.1'
            '';
          });

          jsaddle = wasmPkgs.haskell.lib.overrideCabal hprev.jsaddle (old: {
            prePatch = (old.prePatch or "") + ''
              substituteInPlace jsaddle.cabal \
                --replace-fail 'time >=1.5.0.1 && <1.15' 'time >= 1.5.0.1'
            '';
          });

          # reflex-dom-core-0.8.1.4 bounds template-haskell < 2.24 but GHC 9.14 ships 2.24.
          reflex-dom-core = wasmPkgs.haskell.lib.overrideCabal hprev.reflex-dom-core (old: {
            prePatch = (old.prePatch or "") + ''
              substituteInPlace reflex-dom-core.cabal \
                --replace-fail 'template-haskell >= 2.12.0 && < 2.24' 'template-haskell >= 2.12.0'
            '';
          });

          # Two fixes for jsaddle-wasm-0.1.2.1 under GHC 9.14:
          # 1. cabal2nix omits `if arch(wasm32)` conditional deps, so parser-regex
          #    never reaches the package DB that cabal configure sees.
          # 2. jailbreak-cabal 1.4.1 doesn't strip bounds from `||` constraints,
          #    leaving `ghc-experimental ^>=0.1 || >=9.1000 && <9.1300` unsatisfied
          #    by the installed 9.1401.0; we patch the cabal file in prePatch instead.
          jsaddle-wasm = wasmPkgs.haskell.lib.overrideCabal hprev.jsaddle-wasm (old: {
            libraryHaskellDepends = (old.libraryHaskellDepends or [ ]) ++ [ hfinal.parser-regex ];
            prePatch = (old.prePatch or "") + ''
              substituteInPlace jsaddle-wasm.cabal \
                --replace-fail 'ghc-experimental ^>=0.1 || >=9.1000 && <9.1300' 'ghc-experimental >= 0.1'
            '';
          });

          common = hfinal.callCabal2nix "common" ./common { };

          frontend = hfinal.callCabal2nix "frontend" ./frontend { };
        }
      );

      # CSS + FontAwesome npm deps (Tailwind, PostCSS, etc.)
      diverk-npm-deps = pkgs.buildNpmPackage {
        name = "diverk-npm-deps";
        src = ./static/src;
        npmDepsHash = "sha256-DDKNL2xJMPG7BpsN1Nnpz4EVnOL+PeirStMWkwORg5Y=";
        dontBuild = true;
        installPhase = "mkdir $out && cp -r node_modules $out/node_modules";
      };

      # Frontend JS bundling deps (esbuild + @bjorn3/browser_wasi_shim).
      diverk-frontend-deps = pkgs.buildNpmPackage {
        name = "diverk-frontend-deps";
        src = ./frontend;
        npmDepsHash = "sha256-juxYJBlvRu7U5ZiuyLA8dGSY+8v8OQ4J38M8/m3jqlc=";
        dontBuild = true;
        installPhase = "mkdir $out && cp -r node_modules $out/node_modules";
      };

      # Capacitor CLI + Android runtime deps.
      diverk-mobile-deps = pkgs.buildNpmPackage {
        name = "diverk-mobile-deps";
        src = ./mobile;
        npmDepsHash = "sha256-qnHbGGp1JUeD5PvLCAWXhklaXNE1S7SgZCSRxR0DXk8=";
        dontBuild = true;
        installPhase = "mkdir $out && cp -r node_modules $out/node_modules";
      };

      # --- Android SDK (lazy: only built when the `android` shell is used) ---
      # Versions match what a Capacitor 6/7 project targets (AGP 8, JDK 17,
      # compileSdk 34). Re-align these with mobile/android after `cap add`.
      androidSdk =
        (pkgs.androidenv.composeAndroidPackages {
          platformVersions = [ "34" ];
          buildToolsVersions = [ "34.0.0" ];
        }).androidsdk;
      androidSdkRoot = "${androidSdk}/libexec/android-sdk";

    in
    let
      systemPackages = rec {
        diverkStatic = pkgs.stdenv.mkDerivation {
          name = "diverk-static";
          src = ./.;
          nativeBuildInputs = [ pkgs.nodejs ];
          buildPhase = ''
            ln -s ${diverk-npm-deps}/node_modules static/src/node_modules
            bash static/build-css.sh $out
          '';
          dontInstall = true;
        };

        diverk-wasm = pkgs.stdenv.mkDerivation {
          name = "diverk-wasm";
          nativeBuildInputs = [ wasmToolchain pkgs.nodejs pkgs.esbuild ];
          dontUnpack = true;
          buildPhase = ''
            mkdir -p $out
            wasmBin=$(find ${wasmHaskellPkgs.frontend} -name '*.wasm' | head -1)
            cp "$wasmBin" $out/bin.wasm
            "$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" \
              --input $out/bin.wasm --output $out/ghc_wasm_jsffi.js
            cp ${./frontend/index.html} $out/index.html
            cp ${./frontend/index.js} index.js
            ln -s ${diverk-frontend-deps}/node_modules node_modules
            esbuild index.js --bundle --format=esm \
              --external:./ghc_wasm_jsffi.js --outfile=$out/index.js
          '';
          dontInstall = true;
        };

        default = pkgs.symlinkJoin {
          name = "diverk";
          paths = [ diverk-wasm diverkStatic ];
        };

        android-release-aab = pkgs.stdenv.mkDerivation {
          pname = "diverk-android";
          version = "1.0";
          src = ./mobile;
          nativeBuildInputs = [ pkgs.gradle pkgs.nodejs pkgs.jdk17 androidSdk ];
          ANDROID_SDK_ROOT = androidSdkRoot;
          ANDROID_HOME = androidSdkRoot;
          JAVA_HOME = "${pkgs.jdk17.home}";
          GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${androidSdkRoot}/build-tools/34.0.0/aapt2";

          mitmCache = android-gradle-deps;

          # nixDownloadDeps hits variant-ambiguity on Android subproject test
          # configurations; assembleRelease resolves the same Maven artifacts
          # without touching test configs and is what the actual build does anyway.
          gradleUpdateTask = "bundleRelease";

          configurePhase = ''
            runHook preConfigure
            export GRADLE_OPTS="$GRADLE_OPTS -Dorg.gradle.native.dir=$TMPDIR/gradle-native"
            export ANDROID_USER_HOME="$TMPDIR/android-user-home"
            mkdir -p "$ANDROID_USER_HOME"
            rm -rf android/app/src/main/assets/public \
                   android/capacitor-cordova-android-plugins \
                   android/capacitor.settings.gradle
            ln -s ${diverk-mobile-deps}/node_modules node_modules
            mkdir -p ../frontend
            cp -rL --no-preserve=mode ${default} ../frontend/dist
            npm run copy
            runHook postConfigure
          '';

          preBuild = "cd android";

          buildPhase = ''
            runHook preBuild
            gradle bundleRelease
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp build/android/app/outputs/bundle/release/app-release.aab $out/
            runHook postInstall
          '';
        };
      };
      # Single MITM cache derivation: records Gradle/Maven HTTP traffic when
      # `update-android-deps` runs (record mode), then replays it in the nix
      # sandbox during `nix build .#android-release-aab` (replay mode).
      # useBwrap = false: bwrap's --clearenv drops /etc, breaking cap sync's
      # os.userInfo() call inside the update script.
      android-gradle-deps = pkgs.gradle.fetchDeps {
        pkg = systemPackages.android-release-aab;
        data = ./mobile/deps.json;
        useBwrap = false;
      };
    in
    {
      packages.${system} = systemPackages;

      devShells.${system} = {
        # `nix develop` — WASM + Android build shell.
        # shellFor pre-populates GHC_PACKAGE_PATH with WASM-compiled packages
        # so wasm32-wasi-cabal only needs to compile common/ and frontend/.
        default = pkgs.mkShell {
          name = "diverk";
          inputsFrom = [
            (wasmHaskellPkgs.shellFor {
              packages = ps: [ ps.frontend ps.common ];
              nativeBuildInputs = [ wasmToolchain ];
            })
          ];

          packages = [
            pkgs.nodejs
            pkgs.git
            pkgs.just
            pkgs.esbuild
            pkgs.jdk17
            androidSdk
            pkgs.gradle
            (pkgs.writeShellScriptBin "update-android-deps"
              "exec ${android-gradle-deps.updateScript}")
          ];

          ANDROID_SDK_ROOT = androidSdkRoot;
          ANDROID_HOME = androidSdkRoot;

          shellHook = ''
            ln -sfn ${diverk-npm-deps}/node_modules static/src/node_modules
            ln -sfn ${diverk-frontend-deps}/node_modules frontend/node_modules
            ln -sfn ${diverk-mobile-deps}/node_modules mobile/node_modules
            export JAVA_HOME="${pkgs.jdk17.home}"
            export GRADLE_OPTS="-Dorg.gradle.project.android.aapt2FromMavenOverride=${androidSdkRoot}/build-tools/34.0.0/aapt2 $GRADLE_OPTS"
          '';
        };

      };
    };
}
