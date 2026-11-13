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
        lambdaPkg = pkgs.haskell.lib.doCheck (hp.callCabal2nix "lambda" ./. {});
        lambdaCov = pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.doCoverage lambdaPkg) (drv: {
          postCheck = (drv.postCheck or "") + ''
            echo "=== HPC SUMMARY ==="
            hpc report $(find dist -name "*.tix" | head -n1) --hpcdir=dist/hpc/vanilla/mix/lambda-0.1.0.0 --hpcdir=dist/hpc/vanilla/mix/lambda-test --exclude=Main
            echo "=== HPC PER-MODULE ==="
            hpc report $(find dist -name "*.tix" | head -n1) --hpcdir=dist/hpc/vanilla/mix/lambda-0.1.0.0 --hpcdir=dist/hpc/vanilla/mix/lambda-test --exclude=Main --per-module
          '';
        });
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
          coverage = lambdaCov;
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
