# Diverk

Diverk is a simple app that allows you to browse a GitHub repository on your 
Android device. It renders markdown files and is thus mainly used to access 
personal notes or documentation written in this format.

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

The app is written in Haskell using the [obelisk][obelisk] framework. 

Start a shell with `obelisk` command:

```bash
$ ./shell-obelisk.sh
```

Run locally the app with:

```bash
$ ob run
```

and go to `http://localhost:8000` to access it.

To hack on the code, [haskell-langage-server][haskell-langage-server] can be 
made available in the shell with:

```bash
$ ob shell
```

The Android app can be built with:

```bash
$ nix-build -A android.frontend -o result-android
```

Plug your phone with USB debugging on, then run:

```bash
$ ./result-android/bin/deploy
```

to install the app on the device.

See also the [obelisk documentation][obelisk-android] for more details.

[github-search-code]: https://docs.github.com/en/rest/search/search#search-code
[github-rate-limit]: https://docs.github.com/en/rest/overview/resources-in-the-rest-api#rate-limiting
[haskell-langage-server]: https://github.com/haskell/haskell-language-server
[obelisk-android]: https://github.com/obsidiansystems/obelisk#android
[obelisk]: https://github.com/obsidiansystems/obelisk
[tokens]: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#fine-grained-personal-access-tokens

