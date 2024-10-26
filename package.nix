{ mkDerivation, aeson, base, binary, bytestring, containers, lens
, lens-aeson, lib, matrix-client, network, text, utf8-string
}:
mkDerivation {
  pname = "midymidy-factorio-webservice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring containers lens lens-aeson
    matrix-client network text utf8-string
  ];
  homepage = "https://github.com/nicball/midymidy-factorio-webservice";
  license = lib.licenses.agpl3Plus;
  mainProgram = "midymidy-factorio-webservice";
}
