{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            zlib
            haskell.compiler.ghc9122
          ];
        };
      });
}
