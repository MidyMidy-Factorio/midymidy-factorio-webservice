{
  description = "Factorio Telegram Bot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    let overlay = self: super: {
      midymidy-factorio-webservice = self.callPackage ./default.nix {};
    }; in
    flake-utils.lib.eachDefaultSystem (system: rec {
      packages =
        let pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; }; in
        builtins.intersectAttrs (overlay 42 42) pkgs // {
          default = packages.midymidy-factorio-webservice;
        };
      devShells.default =
        with nixpkgs.legacyPackages.${system};
        mkShell {
          packages = [ haskell-language-server ];
          inputsFrom = [ packages.default ];
        };
    }) // {
      overlays.default = overlay;
    };
}
