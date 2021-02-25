{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [ cabal-install ghc ghcid hlint ];
}
