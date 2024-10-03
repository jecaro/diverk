{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
, androidIsRelease ? false
}:
with obelisk;
project ./. ({ pkgs, ... }: {
  shellToolOverrides = self: super: {
    haskell-language-server = pkgs.haskell.packages.ghc8107.haskell-language-server;
    implicit-hie = pkgs.haskell.packages.ghc8107.implicit-hie;
    hlint = pkgs.haskell.packages.ghc8107.hlint;
  };

  overrides = self: super: {
    # The version (v1.1.1) shipped with the reflex platform does not build. We
    # use a more recent version.
    lens-aeson = pkgs.haskell.lib.doJailbreak (self.callHackageDirect
      {
        pkg = "lens-aeson";
        ver = "1.1.3";
        sha256 = "W5/NtS8z3AnJ5fHfKStDiRAAfvwT6cz+qpzYP9oJj6A=";
      }
      { });
    # The version (v0.2.2) shipped with the reflex platform does not build
    # with ghc-js.
    commonmark = pkgs.haskell.lib.doJailbreak (self.callHackageDirect
      {
        pkg = "commonmark";
        ver = "0.1.1.4";
        sha256 = "sha256-+pF0wrLCeRlPYzxX5b30KjpBTxRtu+K5ORlZtSVSf0k=";
      }
      { });
  };

  staticFiles = import ./static { inherit pkgs; };

  android = {
    applicationId = "org.jecaro.diverk";
    displayName = "Diverk";
    isRelease = androidIsRelease;
    resources = reflex-platform.android.buildIcons {
      src = ./assets/icon.png;
    };
    version = {
      code = "4";
      name = "1.4";
    };
  } // pkgs.lib.optionalAttrs androidIsRelease {
    gradleTask = "bundleRelease";
  };

  ios.bundleIdentifier = "org.jecaro.diverk";
  ios.bundleName = "Diverk";
})
