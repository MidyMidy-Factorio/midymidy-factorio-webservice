{
  description = "Factorio Telegram Bot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      packages = rec {
        midymidy-factorio-webservice =
          with nixpkgs.legacyPackages."${system}";
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
          };
        default = midymidy-factorio-webservice;
      };
      devShells.default =
        with nixpkgs.legacyPackages.${system};
        mkShell {
          packages = [ haskell-language-server ];
          inputsFrom = [ packages.default ];
        };
    });
}
