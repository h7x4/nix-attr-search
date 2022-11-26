{ pkgs, ... }:
pkgs.writeScriptBin "nix-attr-search-hlint" ''
  find -name '*.hs' -exec '${pkgs.hlint}/bin/hlint' {} \;
''
