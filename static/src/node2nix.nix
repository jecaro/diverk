(import (builtins.fetchTarball {
     name = "nixpkgs-unstable_2020-11-18";
     url = "https://github.com/nixos/nixpkgs/archive/4f3475b113c93d204992838aecafa89b1b3ccfde.tar.gz";
     sha256 = "158iik656ds6i6pc672w54cnph4d44d0a218dkq6npzrbhd3vvbg";
   }) {}).nodePackages.node2nix
