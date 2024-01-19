{
  description = "Factorio Telegram Bot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      packages = rec {
        midymidy-factorio-webservice = nixpkgs.legacyPackages."${system}".callPackage ./default.nix {};
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
