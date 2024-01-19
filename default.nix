{ stdenv
, haskellPackages
}:

stdenv.mkDerivation {
  pname = "midymidy-factorio-webservice";
  version = "0.1.0";
  src = ./.;
  buildInputs = [ (haskellPackages.ghcWithPackages (p: with p; [
    scotty wai warp text bytestring
    utf8-string binary aeson network
    lens lens-aeson http-conduit
  ]) ) ];
  buildPhase = ''
    runHook preBuild
    ghc -O2 ./src/*.hs -o midymidy-factorio-webservice
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp ./midymidy-factorio-webservice $out/bin/
    runHook postInstall
  '';
}
