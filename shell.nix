{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          parsec text unicode-transforms
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "cedict-tools-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
