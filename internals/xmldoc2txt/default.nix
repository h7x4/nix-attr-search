{ pkgs, compiler ? "ghc924", ... }:
pkgs.writers.writeHaskellBin "xmldoc2txt" {
  libraries = with pkgs.haskellPackages; [ tagsoup ansi-terminal split text ];
} (builtins.readFile ./xmldoc2txt.hs)
