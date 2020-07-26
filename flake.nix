{
  description = "A simple utility to deal with media files tags";

  inputs.nixpkgs = {
    url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      packages.x86_64-linux.tag = import ./default.nix {
        inherit pkgs;
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.tag;

      overlay = final: prev: {
        tag = import ./default.nix {
          pkgs = final;
        };
      };

      devShell = import ./shell.nix {
        inherit pkgs;
      };
    };
}
