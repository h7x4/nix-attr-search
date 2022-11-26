{ pkgs ? import <nixpkgs> { }, nixpkgs ? <nixpkgs>
, home-manager ? <home-manager>, ... }: rec {
  # Applications
  home-manager-search = pkgs.callPackage ./home-manager-search.nix {
    inherit home-manager;
    inherit (internals) json2nix;
  };

  nix-option-search =
    pkgs.callPackage ./nix-option-search.nix { inherit nixpkgs; };

  nix-package-search = pkgs.callPackage ./nix-package-search.nix { };

  internals = {
    # Data sources
    home-manager-json = home-manager.packages.${system}.docs-json;
    nix-options-json =
      (import "${nixpkgs}/nixos/release.nix" { inherit nixpkgs; }).options;
    nix-packages-json = pkgs.emptyFile;

    # Internal Tools
    json2nix = pkgs.callPackage ./internals/json2nix { };
  };
}
