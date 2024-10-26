{ mkDerivation, aeson, base, binary, bytestring, http-client
, http-conduit, lens, lens-aeson, lib, network, scotty, text
, utf8-string, wai, warp
}:
mkDerivation {
  pname = "midymidy-factorio-webservice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring http-client http-conduit lens
    lens-aeson network scotty text utf8-string wai warp
  ];
  homepage = "https://github.com/nicball/midymidy-factorio-webservice";
  license = lib.licenses.agpl3Plus;
  mainProgram = "midymidy-factorio-webservice";
}
