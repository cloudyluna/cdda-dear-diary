{
  description = "A top down game";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        {
          devShells.default = with pkgs; pkgs.mkShell rec {
            nativeBuildInputs = [
              haskell.compiler.ghc96
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
              
              ghciwatch
              haskellPackages.cabal-gild
            ];
        };
      }
    );
}

