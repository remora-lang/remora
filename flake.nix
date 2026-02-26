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
        { self', pkgs, config,... }:
        let
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
        in
        {
          haskellProjects.default = {
            packages.sbv.source = inputs.sbv;
            settings.sbv.check = false; # sbv tests fail
            devShell = {
              hlsCheck.enable = true;

              mkShellArgs = {
                    packages = with pkgs; [
                      z3
                      nixfmt
                      futhark
                      git gitRepo gnupg autoconf curl
                      procps gnumake util-linux m4 gperf unzip
                      zlib
                      ncurses5
                      stdenv.cc
                      binutils
                    ];
                  };
            };

          };

          packages.default = self'.packages.remora;

          packages = {
            inherit remora-wrapped;

            remora-all = pkgs.buildEnv {
              name = "remora-all";
              paths = [
                remora-wrapped
                self'.packages.remora-docs
              ];
            };

            remora-docs = pkgs.stdenv.mkDerivation {
              pname = "remora-docs";
              version = "0.1";
              src = ./.;
              nativeBuildInputs = [ pkgs.sphinx ];
              phases = [
                "buildPhase"
                "installPhase"
              ];
              buildPhase = ''
                mkdir -p "docs/_build/html"
                sphinx-build -b html "$src/docs" "docs/_build/html"
              '';
              installPhase = ''
                mkdir -p "$out/docs"
                cp -r docs/_build/html "$out/docs/html"
              '';
            };
          };

          devShells.cuda = pkgs.mkShell {
            name = "cuda";
            inputsFrom = [ config.devShells.default ];

            packages = with pkgs; [
              cudaPackages.cudatoolkit
            ];

            shellHook = ''
              export CUDA_PATH=${pkgs.cudaPackages.cudatoolkit}
              export LD_LIBRARY_PATH=/run/opengl-driver/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
                            '';
          };
        };
    };
}
