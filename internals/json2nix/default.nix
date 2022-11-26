{ pkgs, ... }:
let
  overlayedPkgs = pkgs.extend
    (pkgs.callPackage ./haskell-overlay.nix { compiler = "ghc942"; });
in overlayedPkgs.writers.writeHaskellBin "json2nix" {
  libraries = with overlayedPkgs.haskellPackages; [ aeson nixfmt extra text ];
} (builtins.readFile ./json2nix.hs)
