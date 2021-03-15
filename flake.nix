{
  description = "pg-sse flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2009";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          pgsse =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8104";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.pgsse.flake {};
    in flake // {
      defaultPackage = flake.packages."pg-sse:exe:pg-sse";


      devShell = pkgs.pgsse.shellFor {

        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
        };
        withHoogle = false;
        exactDeps = true;

        buildInputs = with pkgs;
          [ neovim ];
      };
    });
}
