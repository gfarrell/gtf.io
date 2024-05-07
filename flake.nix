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
          djot = hfinal.callCabal2nix "djot" (builtins.fetchGit {
            url = "git@github.com:gfarrell/djoths.git";
            rev = "bad299275d650e71b7915ae6da4cfef6a869ab1a";
          }) {};
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
