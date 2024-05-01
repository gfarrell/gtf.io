{
  description = "gtf.io website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://whetstone.private.storage/jcalderone/hs-flake-utils.git?ref=main";
    hs-flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc947";
    # ^ `nix-env -f "<nixpkgs>" -qaP -A haskell.compiler` suggests we should have 947 but nix is complaining
  in
    ulib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./.;
        compilerVersion = ghcVersion;
        packageName = "gtf-website";
        hsPkgsOverrides = hfinal: hprev: {
          # https://ryantm.github.io/nixpkgs/languages-frameworks/haskell/#haskell-derivation-args
          djot = hfinal.mkDerivation {
            src = "https://github.com/gfarrell/djoths.git";
          };
        };
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs:
          with pkgs; [
            zlib
          ];
      };
      packages = hslib.packages {};
      apps.release = hslib.apps.release {};
    });
}
