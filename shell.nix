let
  obeliskSrc = fetchGit {
    url = "https://github.com/obsidiansystems/obelisk.git";
    ref = "refs/tags/v1.2.0.0";
  };
in
(import obeliskSrc { }).shell
