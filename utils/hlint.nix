{ pkgs, ... }:
pkgs.writeScriptBin "nix-attrs-search-hlint" ''
  find -name '*.hs' -exec '${pkgs.hlint}/bin/hlint' {} \;
''
