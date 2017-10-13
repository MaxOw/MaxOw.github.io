{ nixpkgs ? import <nixpkgs> {}, compiler }:
nixpkgs.haskell.packages.${compiler}.callPackage ./blog.nix { }
