let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.unstable { }
}:
let
  tag = import ./default.nix {
    inherit pkgs;
  };
in
pkgs.haskellPackages.shellFor {
  packages = _: [
    tag
  ];
  withHoogle = true;
}
