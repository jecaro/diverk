# Diverk

[![nix][status-nix-png]][status-nix]

<a href='https://play.google.com/store/apps/details?id=org.jecaro.diverk'>
    <img alt='Get it on Google Play' height='75' 
    src='https://play.google.com/intl/en_us/badges/static/images/badges/en_badge_web_generic.png'/>
</a>

Diverk is a simple app that allows you to browse a GitHub repository on your 
Android device. It renders markdown files and is thus mainly used to access 
personal notes or documentation written in this format.

It is available on [Google Play][play] and on the [web][web].

It is able to access private repositories when given a valid access token. We 
recommend using [fine-grained access tokens][tokens] for this purpose.

Note that the app uses the GitHub API and is thus subject to rate limiting. 
Without a token, the rate limit is 60 requests per hour. It roughly corresponds 
to 60 pages per hour. It should be enough for quickly looking up something. But 
for more intensive use, we recommend using a token.

Using a token increases the rate limit to 5,000 requests per hour. More 
information about rate limits is available in the [GitHub 
documentation][github-rate-limit]. Additionally, to be able to search in a 
repository, a token is mandatory. This uses the [search code 
feature][github-search-code] of the GitHub API. For this specific feature, the 
rate limit is 10 searches per hour.

## Building

The app is written in Haskell with [Reflex][reflex] and compiled to
WebAssembly using the GHC WebAssembly backend. It is a fully
client-side single-page app. There is no backend. The toolchain
(`wasm32-wasi-ghc`, `wasm32-wasi-cabal`, `node`, ...) is provided by a Nix
flake wrapping [ghc-wasm-meta][ghc-wasm-meta].

### Dev workflow - native GHC for fast iteration

The default shell uses a native GHC toolchain with [jsaddle-warp][jsaddle-warp]
to run the app directly in the browser without a WebAssembly compile step.
[ghcid] watches for source changes and reloads the server automatically.

```bash
$ nix develop .#native
$ just native-dev
```

This builds the CSS (Tailwind + FontAwesome) and starts a hot-reloading server
at <http://localhost:3000>. Note that this doesn't work with Firefox, only 
Chrome.

### Dev workflow - WebAssembly

To test the production code path (compiled to WebAssembly):

```bash
$ nix develop .#wasm
$ just wasm-run
```

This builds the CSS, compiles the frontend to WebAssembly, and starts a server
at <http://localhost:3000>. To recompile only the Haskell and skip the CSS
step: `just wasm-dev`.

### Nix build

```bash
$ nix build
```

Produces an optimised, self-contained web app in `result/`. From the `.#wasm`
shell, serve it with:

```bash
$ node serve.mjs result
```

### Android

Set up `mobile/android/keystore.properties` (see
`mobile/android/keystore.properties.example`) for release signing, then:

```bash
$ nix develop .#wasm
$ just android-debug    # debug APK, auto-signed, installable via adb
$ just android-release  # release APK, requires keystore.properties
```

`just android-debug` outputs to
`mobile/android/build/android/app/outputs/apk/debug/app-debug.apk`.
Install with `adb install -r <path>`.

To build a release AAB for the Play Store:

```bash
$ nix build .#android-release-aab
```

[ghc-wasm-meta]: https://gitlab.haskell.org/ghc/ghc-wasm-meta
[ghcid]: https://github.com/ndmitchell/ghcid
[github-rate-limit]: https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting
[github-search-code]: https://docs.github.com/en/rest/search/search#search-code
[jsaddle-warp]: https://hackage.haskell.org/package/jsaddle-warp
[play]: https://play.google.com/store/apps/details?id=org.jecaro.diverk
[reflex]: https://reflex-frp.org
[status-nix-png]: https://github.com/jecaro/diverk/workflows/nix/badge.svg
[status-nix]: https://github.com/jecaro/diverk/actions/workflows/nix-build.yml
[tokens]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#fine-grained-personal-access-tokens
[web]: https://diverk.quillet.org

