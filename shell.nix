{ pkgs }:
pkgs.mkShell {
  packages = with pkgs; [ nixfmt hlint jq bat gomplate ];

  shellHook = let
    format = pkgs.callPackage ./utils/format.nix { };
    hlint = pkgs.callPackage ./utils/hlint.nix { };
    ormolu = pkgs.callPackage ./utils/ormolu.nix { };
  in ''
    alias nasf=${format}/bin/nix-attr-search-format
    alias nashl=${hlint}/bin/nix-attr-search-hlint
    alias nashf=${ormolu}/bin/nix-attr-search-ormolu
    cat << EOF
    [Aliases]
    nasf -> formats all nix files
    nashl -> lint report for all haskell files
    nashf -> format all haskell files
    EOF
  '';
}
