let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.unstable { }
}:
let
  hspkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: with pkgs.haskell.lib; {
      ede = overrideCabal hsuper.ede (
        drv: {
          src = pkgs.fetchFromGitHub {
            owner = "brendanhay";
            repo = "ede";
            rev = "5e0373b8a8c83ff2078a938795e30ec8038d228c";
            sha256 = "1lb0q289p6lrc65adlacdx8xy8hrvcywbf6np7rilqdvvnyvlbgs";
          };
        }
      );
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
