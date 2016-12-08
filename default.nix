{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
with nixpkgs.pkgs.haskell;
lib.overrideCabal (packages.${compiler}.callPackage ./ircTest.nix {})
  (drv: { src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src; })
