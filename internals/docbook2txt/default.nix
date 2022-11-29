{ pkgs, compiler ? "ghc924", ... }:
pkgs.writers.writeHaskellBin "docbook2txt" {
  libraries = with pkgs.haskellPackages; [ tagsoup ansi-terminal split text ];
} (builtins.readFile ./docbook2txt.hs)
