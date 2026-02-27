{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    sbv = {
      url = "github:LeventErkok/sbv";
      flake = false;
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        {
          self',
          lib,
          config,
          system,
          ...
        }:
        let

          pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };
        in
        {
          haskellProjects.default = {
            packages.sbv.source = inputs.sbv;
            settings.sbv.check = false; # sbv tests fail
          };

          devShells.default = lib.mkForce (
            pkgs.mkShell {
              inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
              packages = with pkgs; [
                z3
                futhark
                cudatoolkit
              ];
              shellHook = ''
                export CUDA_PATH="''${CUDA_PATH:+$CUDA_PATH:}${pkgs.cudatoolkit}"

                # Only works on NixOS:
                export LD_LIBRARY_PATH="''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}/run/opengl-driver/lib"
                export LIBRARY_PATH="''${LIBRARY_PATH:+:$LIBRARY_PATH}/run/opengl-driver/lib"
              '';
            }
          );

          packages.default = self'.packages.remora;
        };
    };
}
