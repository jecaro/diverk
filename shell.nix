let
  obeliskSrc = fetchGit {
    url = "https://github.com/obsidiansystems/obelisk.git";
    ref = "release/1.3.0.0";
    rev = "58c04270d606c061e7ffd2f16345e0f451eba600";
  };
in
(import obeliskSrc { }).shell
