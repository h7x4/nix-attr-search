{ pkgs, ... }:
pkgs.writeScriptBin "nix-attr-search-format" ''
  find -name '*.nix' -exec '${pkgs.nixfmt}/bin/nixfmt' {} \;
''
