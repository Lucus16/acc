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
  shellHook = ''
    export PATH=${toString dist-newstyle/build/x86_64-linux/ghc-8.10.4/acc-0.1.0.0/x/splee/build/splee}:$PATH
  '';
}
