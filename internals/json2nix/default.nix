{ pkgs, compiler ? "ghc924", ... }:
let
  overlayedPkgs =
    pkgs.extend (pkgs.callPackage ./haskell-overlay.nix { inherit compiler; });
in overlayedPkgs.writers.writeHaskellBin "json2nix" {
  libraries = with overlayedPkgs.haskellPackages; [ aeson nixfmt extra text ];
} (builtins.readFile ./json2nix.hs)
