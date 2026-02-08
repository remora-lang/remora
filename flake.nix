{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
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
        { self', pkgs, ... }:
        let
          haskellPackages = pkgs.haskell.packages.ghc910.override {
            overrides = self: super: {
              sbv =
                (self.callHackageDirect {
                  pkg = "sbv";
                  ver = "13.5";
                  sha256 = "sha256-9/BvbA6uyI1qJ52fKyaR8fhQp1gkx8yIp8wYS1EOuHk=";
                } { }).overrideAttrs
                  (old: {
                    doCheck = false;
                  });
            };
          };
          remora-wrapped = pkgs.stdenv.mkDerivation {
            name = "remora-wrapped";
            nativeBuildInputs = [ pkgs.makeWrapper ];
            src = self'.packages.remora-lang;
            installPhase = ''
              mkdir -p $out/bin
              makeWrapper $src/bin/remora $out/bin/remora \
                --prefix PATH : "${pkgs.z3}/bin"
            '';
          };
        in
        {
          haskellProjects.default = {
            basePackages = haskellPackages;
            devShell = {
              tools = hp: { z3 = pkgs.z3; };
              hlsCheck.enable = true;
            };
          };

          packages.default = self'.packages.remora-all;

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
        };
    };
}
