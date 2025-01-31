let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.unstable { }
}:
let
  hspkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: with pkgs.haskell.lib; {
      ede = dontCheck
        (unmarkBroken (
          hsuper.ede
        ));
    };
  };

  inherit (hspkgs)
    callCabal2nix
    ;

  inherit (pkgs.haskell.lib)
    justStaticExecutables
    ;

  inherit (pkgs)
    nix-gitignore
    ;

  tag = callCabal2nix "tag" (nix-gitignore.gitignoreSource [ ] ./.) { };
in
justStaticExecutables tag
