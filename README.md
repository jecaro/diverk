# Diverk

<a href='https://play.google.com/store/apps/details?id=org.jecaro.diverk'>
    <img alt='Get it on Google Play' height='75' 
    src='https://play.google.com/intl/en_us/badges/static/images/badges/en_badge_web_generic.png'/>
</a>

Diverk is a simple app that allows you to browse a GitHub repository on your 
Android device. It renders markdown files and is thus mainly used to access 
personal notes or documentation written in this format.

It is available on [google play][play] and on the [web][web].

It is able to access private repositories when given a valid access token. We 
recommend using [fine-grained access tokens][tokens] for this purpose.

Note that the app uses the GitHub API and is thus subject to rate limiting. 
Without a token, the rate limit is 60 requests per hour. It roughly corresponds 
to 60 pages per hour. It should be enough for quickly looking up something. But 
for a more intensive use, we recommend using a token.

Using a token increases the rate limit to 5,000 requests per hour. More 
information about rate limits is available in the [GitHub 
documentation][github-rate-limit]. Additionally, to be able to search in a 
repository, a token is mandatory. This uses the [search code 
feature][github-search-code] of the GitHub API. For this specific feature, the 
rate limit is 10 searches per hour.

## Building

The app is written in Haskell with [Reflex][reflex] and compiled to
WebAssembly using the [GHC WebAssembly backend][ghc-wasm]. It is a fully
client-side single-page app — there is no backend. The toolchain
(`wasm32-wasi-ghc`, `wasm32-wasi-cabal`, `node`, ...) is provided by a Nix
flake wrapping [ghc-wasm-meta][ghc-wasm-meta].

Enter the development shell:

```bash
$ nix develop
```

Build the CSS assets (Tailwind + FontAwesome), then compile the frontend to
WebAssembly and assemble `frontend/dist/`:

```bash
$ (cd static && ./build-css.sh)
$ (cd frontend && ./build.sh)
```

Serve the result and open <http://localhost:8099>:

```bash
$ node serve.mjs
```

For a size-optimized production build (`wizer` + `wasm-opt` + strip):

```bash
$ (cd frontend && ./build.sh -Oz)
```

### Android

Native Android packaging previously relied on Obelisk and is being reworked on
top of the WebAssembly SPA (a WebView wrapper such as Capacitor, or a PWA).

[ghc-wasm-meta]: https://gitlab.haskell.org/ghc/ghc-wasm-meta
[ghc-wasm]: https://www.haskell.org/ghc/blog/20220222-wasm-backend-merged.html
[github-rate-limit]: https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting
[github-search-code]: https://docs.github.com/en/rest/search/search#search-code
[play]: https://play.google.com/store/apps/details?id=org.jecaro.diverk
[reflex]: https://reflex-frp.org
[tokens]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#fine-grained-personal-access-tokens
[web]: https://diverk.quillet.org

