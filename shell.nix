{ ghcVersion ? "ghc901", pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  haskellPackages = if (ghcVersion != null) then
    haskell.packages.${ghcVersion}
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
    ]);

in mkShell {
  buildInputs = [
    cabal-install
    gdb
    ghc
    ghcid
    haskell-language-server
    hlint
    mold
    nasm
  ];

  LD = "${mold}/bin/mold";
  LIBC = glibc;
}
