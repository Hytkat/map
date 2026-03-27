{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.xz";
    nixpkgs-lib.follows = "nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      flake-parts,
      opam-nix,
      opam-repository,
      treefmt-nix,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      perSystem =
        {
          pkgs,
          system,
          self',
          ...
        }:
        let
          on = opam-nix.lib.${system};

          basePackagesQuery = {
            ocaml-base-compiler = "*";
            fold = "*";
          };

          devPackagesQuery = {
            mapv = "*";
            ocamlformat = "*";
            ocaml-lsp-server = "*";
          };

          scope = on.buildOpamProject' { repos = [ opam-repository ]; } ./. (
            basePackagesQuery // devPackagesQuery
          );

          devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
        in
        {
          treefmt = {
            projectRootFile = "flake.nix";
            programs = {
              nixfmt.enable = true;
              ocamlformat = {
                enable = true;
                package = scope.ocamlformat // {
                  meta.mainProgram = "ocamlformat";
                };
              };
            };
          };

          packages = {
            inherit (scope) mapv fold;
            fmt = self'.formatter;
          };

          overlayAttrs = {
            ocamlPackages.mapv = scope.mapv;
            inherit (scope) fold;
          };

          devShells.default = pkgs.mkShell {
            name = "mapv-dev";
            inputsFrom = [ scope.fold ];
            nativeBuildInputs = devPackages;
          };
        };
    };
}
