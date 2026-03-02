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
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem =
        {
          self',
          lib,
          config,
          pkgs,
          system,
          ...
        }:
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          haskellProjects.default = {
            packages.sbv.source = inputs.sbv;
            settings.sbv.check = false;
            devShell = {
              tools = _: { inherit (pkgs) z3 nixfmt futhark cudatoolkit; };
              hlsCheck.enable = true;
              mkShellArgs.shellHook = ''
                export CUDA_PATH="${pkgs.cudaPackages.cudatoolkit}''${CUDA_PATH:+:}$CUDA_PATH"
                export LD_LIBRARY_PATH="/run/opengl-driver/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
                export LIBRARY_PATH="${pkgs.cudaPackages.cudatoolkit}/lib/stubs''${LIBRARY_PATH:+:}$LIBRARY_PATH"
              '';
            };
          };

          packages = {
            remora-wrapped = pkgs.stdenv.mkDerivation {
              name = "remora-wrapped";
              nativeBuildInputs = [ pkgs.makeWrapper ];
              src = self'.packages.remora;
              installPhase = ''
                mkdir -p $out/bin
                makeWrapper $src/bin/remora $out/bin/remora \
                  --prefix PATH : "${pkgs.z3}/bin"
              '';
            };
            remora-docs = pkgs.stdenv.mkDerivation {
              pname = "remora-docs";
              version = "0.1";
              src = ./.;
              nativeBuildInputs = [ pkgs.sphinx ];
              phases = [ "buildPhase" "installPhase" ];
              buildPhase = ''
                mkdir -p "docs/_build/html"
                sphinx-build -b html "$src/docs" "docs/_build/html"
              '';
              installPhase = ''
                mkdir -p "$out/docs"
                cp -r docs/_build/html "$out/docs/html"
              '';
            };
              default = pkgs.buildEnv {
              name = "remora-all";
              paths = [
                self'.packages.remora-wrapped
                self'.packages.remora-docs
              ];
            };
          };
        };
    };
}
