{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          qutesearchProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc925";
              shell.tools = {
                cabal = {};
                hlint = {};
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.qutesearchProject.flake {
      };
    in flake // {
      packages.default = flake.packages."qutesearch:exe:qutesearch";
    });
}
