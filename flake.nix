{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/master";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler-nix-name = "ghc9122";

        overlays = [
          haskellNix.overlay

          # Configure hixProject
          (final: prev: {
            hixProject = compiler-nix-name:
              final.haskell-nix.hix.project {
                src = gitignore.lib.gitignoreSource ./.;
                evalSystem = system;
                inherit compiler-nix-name;

                modules = [{
                  # Needed since GHC 9.10
                  packages.unix.components.library.configureFlags = [''-f os-string''];
                }];
              };
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };

      in
        {
          packages = ({
            default = ((pkgs.hixProject compiler-nix-name).flake {}).packages."hi-profile-test:exe:hi-profile-test";
          });

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              zlib
              haskell.compiler.ghc9122
              websocat
            ];
          };
        });
}
