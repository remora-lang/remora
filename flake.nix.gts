{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, utils, ... }: utils.lib.eachDefaultSystem (system: 
    let pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          allowUnfreePredicate = pkgs._cuda.lib.allowUnfreeCudaPredicate;
          cudaCapabilities = [ 8.6 ];
          cudaForwardCompat = true;
          cudaSupport = true;               
      };
      shellInputs =  [ (pkgs.haskellPackages.ghcWithPackages (hpkgs: with pkgs; [ 
        hpkgs.cabal-install 
        hpkgs.haskell-language-server
        hpkgs.stack
      ]))
                       pkgs.futhark
                       pkgs.cudaPackages_13.cudatoolkit
                       pkgs.cudaPackages_13.cudnn
                       pkgs.linuxPackages.nvidiaPackages.production
                       pkgs.z3 ];
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = shellInputs;
        # Check that haskell-language-server works
        # hlsCheck.enable = true; # Requires sandbox to be disabled
        shellHook = ''
              SHELL=${pkgs.bashInteractive}/bin/bash
              export PS1="haskell.nix:\W: "
            '';
      };
    }
  );
}
