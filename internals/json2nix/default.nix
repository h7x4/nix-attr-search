{ pkgs, ... }:
pkgs.writers.writeHaskellBin "json2nix" {
  libraries = with pkgs.haskellPackages; [ aeson ];
} (builtins.readFile ./json2nix.hs)
