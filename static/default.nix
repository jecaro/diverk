{ pkgs }:
let
  # The nixified node project was generated from a package.json file in src using node2nix
  # See https://github.com/svanderburg/node2nix#using-the-nodejs-environment-in-other-nix-derivations
  nodePkgs = (pkgs.callPackage ./src {
    inherit pkgs;
    nodejs = pkgs.nodejs-14_x;
  }).shell.nodeDependencies;

  # The frontend source files have to be passed in so that tailwind's purge option works
  # See https://tailwindcss.com/docs/optimizing-for-production#removing-unused-css
  frontendSrcFiles = ../frontend;

in
pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./src;
  buildInputs = [ pkgs.nodejs ];
  installPhase = ''
    mkdir -p $out/css

    # Setting up the node environment:
    ln -s ${nodePkgs}/lib/node_modules ./node_modules
    export PATH="${nodePkgs}/bin:$PATH"

    # We make the frontend haskell source files available here:
    # This corresponds to the path specified in tailwind.config.js
    ln -s ${frontendSrcFiles} frontend

    # Run the postcss compiler:
    postcss css/styles.css -o $out/css/styles.css
  '';
}
