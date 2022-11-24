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
      in builtins.mapAttrs toApp self.packages.${system};

      hydraJobs = with pkgs.lib;
        mapAttrs' (name: value: nameValuePair name { ${system} = value; })
        self.packages.${system};

      packages.${system} = {
        home-manager-search =
          pkgs.callPackage ./home-manager-search.nix { inherit home-manager; };
        nix-option-search = pkgs.callPackage ./nix-option-search.nix { };
        nix-package-search = pkgs.callPackage ./nix-package-search.nix { };
      };

      overlays.default = final: prev: prev // self.packages.${system};

      devShells.${system}.default =
        pkgs.mkShell { packages = with pkgs; [ nixfmt ]; };
    };
}
