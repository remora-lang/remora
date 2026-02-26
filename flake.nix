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
                linuxPackages.nvidia_x11
              ];
              shellHook = ''
                export CUDA_PATH="${pkgs.cudaPackages.cudatoolkit}''${CUDA_PATH:+:}$CUDA_PATH"
                export LD_LIBRARY_PATH="/run/opengl-driver/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
              '';

            }
          );

          packages.default = self'.packages.remora;
        };
    };
}
