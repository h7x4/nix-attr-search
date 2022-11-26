{ pkgs, ... }:
pkgs.writeScriptBin "nix-attr-search-ormolu" ''
  find -name '*.hs' -exec '${pkgs.ormolu}/bin/ormolu' --mode inplace {} \;
''
