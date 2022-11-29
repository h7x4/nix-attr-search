{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      apps.${system} = let
        toApp = _: pkg: {
          type = "app";
          program = toString pkg;
        };
      in builtins.mapAttrs toApp {
        inherit (self.packages.${system})
          home-manager-search nix-option-search nix-package-search;
      } // {
        nix2json = {
          type = "app";
          program = "${self.packages.${system}.nix2json}/bin/nix2json";
        };
        docbook2txt = {
          type = "app";
          program = "${self.packages.${system}.docbook2txt}/bin/docbook2txt";
        };
      };

      hydraJobs = with pkgs.lib;
        mapAttrs' (name: value: nameValuePair name { ${system} = value; })
        self.packages.${system};

      packages.${system} = {
        # Applications
        home-manager-search =
          pkgs.callPackage ./searchers/home-manager-search.nix {
            inherit home-manager;
            inherit (self.packages.${system}) json2nix docbook2txt;
            defaultManualPath =
              let pkg = self.packages.${system}.home-manager-json;
              in "${pkg}/share/doc/home-manager/options.json";
          };
        nix-option-search = pkgs.callPackage ./searchers/nix-option-search.nix {
          inherit nixpkgs;
          inherit (self.packages.${system}) json2nix docbook2txt;
          defaultManualPath =
            let pkg = self.packages.${system}.nix-options-json;
            in "${pkg}/share/doc/nixos/options.json";
        };
        nix-package-search =
          pkgs.callPackage ./searchers/nix-package-search.nix { };
        nix-lib-search = pkgs.callPackage ./searchers/nix-lib-search.nix { };
        nur-package-search =
          pkgs.callPackage ./searchers/nur-package-search.nix { };

        # Data sources
        home-manager-json = home-manager.packages.${system}.docs-json;
        nix-options-json =
          (import "${nixpkgs}/nixos/release.nix" { inherit nixpkgs; }).options;
        # nix-packages-json = pkgs.emptyFile;

        # Internal Tools
        json2nix =
          pkgs.callPackage ./internals/json2nix { compiler = "ghc924"; };
        docbook2txt =
          pkgs.callPackage ./internals/docbook2txt { compiler = "ghc924"; };
      };

      overlays.default = _: prev: prev // self.packages.${system};

      devShells.${system}.default = pkgs.callPackage ./shell.nix { };
    };
}
