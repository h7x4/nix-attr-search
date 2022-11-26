{ pkgs, ... }:
pkgs.writeScriptBin "nix-attrs-search-ormolu" ''
  find -name '*.hs' -exec '${pkgs.ormolu}/bin/ormolu' --mode inplace {} \;
''
