{ pkgs, ... }:
pkgs.writeScriptBin "nix-attrs-search-format" ''
  find -name '*.nix' -exec '${pkgs.nixfmt}/bin/nixfmt' {} \;
''
