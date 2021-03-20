{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      containers
      filepath
      megaparsec
      mtl
      parser-combinators
      process
      text
    ]);

in mkShell {
  buildInputs = [ cabal-install ghc ghcid haskell-language-server hlint ];
}
