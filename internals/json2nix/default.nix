{ pkgs, ... }:
let
  overlayedPkgs = pkgs.extend
    (pkgs.callPackage ./haskell-overlay.nix { compiler = "ghc924"; });
in overlayedPkgs.writers.writeHaskellBin "json2nix" {
  libraries = with overlayedPkgs.haskellPackages; [ aeson nixfmt extra text ];
} (builtins.readFile ./json2nix.hs)
