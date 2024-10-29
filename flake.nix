{
  description = "nixless-pkgs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };

    nix = {
      url = "github:NixOS/nix/2.24.3";
    };
  };

  outputs = inputs @ { self, nixpkgs, ... }: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      inputs.pre-commit-hooks-nix.flakeModule
    ];

    systems = inputs.nixpkgs.lib.systems.flakeExposed;

    perSystem =
      { config
      , self'
      , inputs'
      , lib
      , pkgs
      , system
      , ...
      }:
      let
        hls-alias = pkgs.writeShellScriptBin "haskell-language-server" ''
          ${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper "$@"
        '';
      in
      {
        pre-commit.settings = {
          hooks = {
            nixpkgs-fmt.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
          };
        };

        devShells.default = pkgs.mkShell {
          shellHook = config.pre-commit.installationScript;
          buildInputs = [
            inputs'.nix.packages.nix
            inputs'.nix.packages.nix.dev
          ];
          nativeBuildInputs = [
            pkgs.fd
            pkgs.haskell-language-server
            pkgs.ghc
            pkgs.cabal-install
            pkgs.zlib
            pkgs.pkg-config
            pkgs.gf
            hls-alias
          ];
        };
      };
  };
}
