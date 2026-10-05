{
  description = "build environment for tree-sitter-remora";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # default development shell: do `nix develop`
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.clang
          pkgs.nodejs_22
          pkgs.tree-sitter
        ];
        shellHook = ''
            SHELL=${pkgs.bashInteractive}/bin/bash
            export PATH=${pkgs.qemu}/bin:$PATH
            export PS1="nix:\W: "
          '';
      };
    }
  );
}
