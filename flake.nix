{
  description = "gtf.io website";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      inputs.haskell-flake.flakeModule
      inputs.pre-commit-hooks.flakeModule
    ];
    systems = [ "x86_64-linux" "aarch64-linux" ];

    perSystem = { config, inputs', self', ... }: {
      haskellProjects.default = {
        devShell.mkShellArgs.shellHook = config.pre-commit.installationScript;
        defaults.devShell.tools = hp: with hp; {
          inherit
            cabal-install
            ghcid
            haskell-language-server;
        };
        packages = {
          djot.source = inputs'.nixpkgs.legacyPackages.fetchFromGitHub {
            owner = "gfarrell";
            repo = "djoths";
            rev = "bad299275d650e71b7915ae6da4cfef6a869ab1a";
            hash = "sha256-Ud+BhyifBnvQK7gPK5ruT3GHfIrZFSCVXmUjB7HhnNw=";
          };
        };
      };

      pre-commit.settings.hooks = {
        cabal-fmt.enable = true;
        hlint.enable = false; # hlint 3.6.1 is the latest available (need 3.8) due to  issue #1531
        nixpkgs-fmt.enable = true;
        fourmolu.enable = true;
      };

      packages.default = self'.packages.gtf-website;
    };
  };
}
