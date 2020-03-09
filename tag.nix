{ mkDerivation, base, ede, htaglib, optparse-applicative, protolude
, stdenv, text
}:
mkDerivation {
  pname = "tag";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ede htaglib optparse-applicative protolude text
  ];
  executableHaskellDepends = [ base protolude ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
