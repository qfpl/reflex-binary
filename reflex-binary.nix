{ mkDerivation, array, base, binary, bytestring, containers, stdenv
}:
mkDerivation {
  pname = "reflex-binary";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base binary bytestring containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
