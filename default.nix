{ pkgs }:
let
  haskellPkgs = pkgs.haskell.packages.ghc902;
in
  haskellPkgs.callCabal2nix "hafka" ./. {}
