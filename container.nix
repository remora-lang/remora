{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/dbd5b7af3e5b9382e291662d18d6eb9dd5f24ccb.tar.gz") {} }:

let
  cuda = pkgs.dockerTools.pullImage {
    imageName = "nvidia/cuda";
    imageDigest = "sha256:9cf8694a27722418a1f175d90f85d5afb5a728fd4a9907d7f0565efecfa14d32";
    sha256 = "sha256-syFX6qHQU6u3bksMF6kcw7kqIJDP5BuEQBSa4QwwNyE=";
    finalImageName = "nvidia/cuda";
    finalImageTag = "13.1.1-devel-ubuntu24.04";
  };

  remora = (builtins.getFlake (toString ./.)).packages.${builtins.currentSystem}.default;

  remoraRepo = pkgs.fetchgit {
    url = "https://github.com/remora-lang/remora";
    rev = "a6851910d2384b4cc0f2a9a5f722882076a079b2";
    sha256 = "sha256-PsV0gPN3uzSyAwAyXmija1CuUYYNetJEoxlwuYjVxY4=";
  };

in

pkgs.dockerTools.buildLayeredImage {
  name = "remora";
  tag = "remora";
  fromImage = cuda;

contents = pkgs.buildEnv {
  name = "root";
  paths = with pkgs; [ z3 futhark remora ];
  pathsToLink = [ "/bin" ];
};


  extraCommands = ''
    mkdir remora
    cp -r ${remoraRepo}/* remora/.
  '';

  config = {
    WorkingDir = "/remora";
    Env = [
      "CPATH=/usr/local/cuda/include"
      "C_INCLUDE_PATH=/usr/local/cuda/include"
      "CPLUS_INCLUDE_PATH=/usr/local/cuda/include"
      "LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/cuda/lib64/stubs"
      "LD_LIBRARY_PATH=/usr/local/cuda/lib64:/usr/local/cuda/lib64/stubs"
    ];
  };
}
