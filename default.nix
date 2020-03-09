{ sources ? (import ./nix/sources.nix)
, compiler ? "ghc882"
}:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: {};
        };
      };
    };
  };
  pkgs = (
    import sources.unstable {
      overlays = [
        overlay
      ];
    }
  );
  tag = pkgs.haskellPackages.callCabal2nix "tag" (pkgs.nix-gitignore.gitignoreFilterPure ./.) {};
in
tag
