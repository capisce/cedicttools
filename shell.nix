{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          text unicode-transforms
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "cedict-tools-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
