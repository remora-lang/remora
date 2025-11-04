{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, utils, ... }: utils.lib.eachDefaultSystem (system: 
    let 
      pkgs = nixpkgs.legacyPackages.${system};
      shellInputs =  [ (pkgs.haskellPackages.ghcWithPackages (hpkgs: with pkgs; [ 
        hpkgs.cabal-install 
        hpkgs.haskell-language-server
      ]))
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
