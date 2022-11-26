{ pkgs }:
pkgs.mkShell {
  packages = with pkgs; [ nixfmt hlint jq bat gomplate ];

  shellHook = let
    format = pkgs.callPackage ./utils/format.nix { };
    hlint = pkgs.callPackage ./utils/hlint.nix { };
    ormolu = pkgs.callPackage ./utils/ormolu.nix { };
  in ''
    alias nasf=${format}/bin/nix-attrs-search-format
    alias nashl=${hlint}/bin/nix-attrs-search-hlint
    alias nashf=${ormolu}/bin/nix-attrs-search-ormolu
  '';
}
