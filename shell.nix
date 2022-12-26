{ ghcVersion ? "ghc92", pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = if (ghcVersion != null) then
    pkgs.haskell.packages.${ghcVersion}
  else
    pkgs.haskellPackages;

  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      containers
      filepath
      megaparsec
      mtl
      parser-combinators
      process
      text
      utf8-string
    ]);

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    gdb
    ghc
    haskellPackages.haskell-language-server
    hlint
    nasm
    stylish-haskell
  ];

  LIBC = pkgs.glibc;
}
