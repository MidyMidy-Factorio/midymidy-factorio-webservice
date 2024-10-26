{
  description = "Factorio Telegram Bot";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    let overlay = self: super: {
      midymidy-factorio-webservice = self.haskellPackages.callPackage ./package.nix {};
    }; in
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; }; in rec {
        packages =
          builtins.intersectAttrs (overlay 42 42) pkgs // {
            default = packages.midymidy-factorio-webservice;
          };
        devShells.default =
          pkgs.haskellPackages.shellFor {
            packages = _: [ packages.default ];
            nativeBuildInputs = with pkgs; [ haskell-language-server cabal-install cabal2nix ];
          };
      }) // {
      overlays.default = overlay;
    };
}
