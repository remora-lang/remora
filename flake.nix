{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    futhark = {
      url = "github:diku-dk/futhark";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { nixpkgs, futhark, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          config.packageOverrides = _: {
            futhark = futhark.packages.${system}.default;
          };
        };

      haskellPackagesFor =
        system: pkgs:
        pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            futhark = futhark.packages.${system}.futhark-lib;
            sbv = pkgs.haskell.lib.dontCheck hsuper.sbv_14_0;
            remora = pkgs.haskell.lib.dontCheck (hself.callCabal2nix "remora" ./. { });
          };
        };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          hpkgs = haskellPackagesFor system pkgs;

          remora-wrapped = pkgs.stdenv.mkDerivation {
            name = "remora";
            src = hpkgs.remora;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              mkdir -p $out/bin
              makeWrapper $src/bin/remora $out/bin/remora \
                --prefix PATH : "${pkgs.z3}/bin:${pkgs.futhark}/bin"
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
        in
        {
          inherit (hpkgs) remora;
          inherit remora-wrapped remora-docs;
          default = remora-wrapped;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          hpkgs = haskellPackagesFor system pkgs;
        in
        {
          default = hpkgs.shellFor {
            packages = p: [ p.remora ];
            nativeBuildInputs = [
              hpkgs.cabal-install
              hpkgs.haskell-language-server
              pkgs.pkg-config
              pkgs.zlib
              pkgs.z3
              pkgs.nixfmt
              pkgs.cudatoolkit
              pkgs.futhark
            ];
            shellHook = ''
              export CUDA_PATH="${pkgs.cudaPackages.cudatoolkit}''${CUDA_PATH:+:}$CUDA_PATH"
              export LD_LIBRARY_PATH="/run/opengl-driver/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
              export LIBRARY_PATH="${pkgs.cudaPackages.cudatoolkit}/lib/stubs''${LIBRARY_PATH:+:}$LIBRARY_PATH"
            '';
          };
        }
      );
    };
}
