{ compiler ? "ghc882" }:
(import ./default.nix { inherit compiler; }).env
