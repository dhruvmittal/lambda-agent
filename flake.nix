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
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hp.ghc
            hp.cabal-install
            pkg-config
            zlib
          ];
        };

        packages.default = hp.callCabal2nix "lambda" ./. {};
      }
    );
}
