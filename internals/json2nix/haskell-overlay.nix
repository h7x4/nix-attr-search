{ pkgs, compiler, ... }:
final: prev: {
  haskellPackages = with prev.haskell.lib;
    prev.haskell.packages.${compiler}.override {
      overrides = hpFinal: hpPrev: {
        # Upgrade nixfmts "text" dependency to >= 2
        nixfmt = doJailbreak hpPrev.nixfmt;
      };
    };
}
