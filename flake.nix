{
  description = "lambdA: Systems Engineering Diagnostic & Coding Agent";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghcVersion = "ghc96";
        hp = pkgs.haskell.packages.${ghcVersion};
        lambdaPkg = hp.callCabal2nix "lambda" ./. {};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hp.ghc
            hp.cabal-install
            pkg-config
            zlib
          ];
        };

        packages = {
          lambda = lambdaPkg;
          default = lambdaPkg;
        };

        apps = rec {
          lambda = flake-utils.lib.mkApp {
            drv = lambdaPkg;
          };
          default = lambda;
        };
      }
    );
}
