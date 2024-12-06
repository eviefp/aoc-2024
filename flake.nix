{
  description = "Evie's Advent of Code, 2024";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        treefmt-config = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            fourmolu.package = pkgs.haskell.packages.ghc910.fourmolu;
          };
        };
        treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
        sayHello = pkgs.writeScript "foo"
          ''
            echo "hello"
          '';
      in
      {
        apps.foo = {
          type = "app";
          program = "${sayHello}";
        };

        formatter = treefmt.wrapper;

        checks = {
          format = treefmt.check self;

          lint-haskell = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; } ''
            cd ${./.}
            hlint src test app
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskell.compiler.ghc910
            pkgs.haskell.packages.ghc910.cabal-install
            pkgs.haskell.packages.ghc910.haskell-language-server
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskellPackages.hoogle
            pkgs.hlint
            pkgs.zlib
          ];
        };

        packages.aoc = pkgs.haskell.packages.ghc910.callCabal2nix "evie-aoc" ./. { };
      });
}
