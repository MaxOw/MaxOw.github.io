{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./blog.nix { }
