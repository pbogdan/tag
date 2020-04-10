{ sources ? (import ./nix/sources.nix)
, compiler ? "ghc883"
}:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: with self.haskell.lib; {
            ede = overrideCabal hsuper.ede (
              drv: {
                src = sources."ede-trifecta-2.1";
              }
            );
            text-format = markUnbroken (doJailbreak hsuper.text-format);
          };
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

  inherit (pkgs.haskellPackages)
    callCabal2nix
    shellFor
    ;
  inherit (pkgs.haskell.lib)
    justStaticExecutables
    ;
  inherit (pkgs)
    lib
    nix-gitignore
    ;

  tag = callCabal2nix "tag" (nix-gitignore.gitignoreSource [] ./.) {};
in
justStaticExecutables tag
