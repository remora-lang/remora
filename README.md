# Remora

## Requirements
### Build dependencies
#### Required:
- [`ghc`](https://www.haskell.org/ghc/) (tested with 9.10.3)
- [`cabal`](https://www.haskell.org/cabal/download.html)
- [`z3`](https://github.com/Z3Prover/z3)

### Run dependencies
#### Required:
- [`z3`](https://github.com/Z3Prover/z3)

#### Optional:
- [`futhark`](https://futhark-lang.org) (to compile to C/CUDA)
- [CUDA Toolkit](https://developer.nvidia.com/cuda/toolkit) (to compile to CUDA)
- An Nvidia GPU (to execute binaries produced via the CUDA backend)

### Obtaining dependencies
If you use Nix, the easiest option is to enter a development shell that provides all dependencies via
```
$ nix develop
```

If flakes aren’t enabled on your system, either enable them globally or run:
```
$ nix develop --extra-experimental-features flakes
```

If you don't use Nix, you'll have to install the required dependencies
yourself. In this case, [`GHCup`](https://www.haskell.org/ghcup/) is the
recommended installer to obtain a Haskell installation.

### CUDA requirements
Compiling to CUDA requires the [CUDA
Toolkit](https://developer.nvidia.com/cuda/toolkit). The compiler also needs to
be able to find the CUDA libraries; see
[here](https://futhark.readthedocs.io/en/latest/man/futhark-cuda.html#environment)
for specific environment variable instructions.

## Building
To install `remora` to your local binary directory (defaults to `~/.local/bin`):
```
$ cabal install
```

If you’d rather build without installing:
```
$ cabal build
$ cabal exec -- remora interpret -e "(+ 1 2)"
```

## Running
List available modes and global options:
```
$ remora --help
```

You can also ask for help for a specific mode. For example:
```
$ remora interpret --help
```

### Interpreter
`remora` can interpret either in a REPL
```
$ remora repl
>> (+ 1 2)
3
```
or on the CLI (e.g., if you want to pass a program as a file or via STDIN)
```
$ remora interpret -f tests/basic0.remora
[3 6]
```

### Compiler
The compiler is in early stages. At the moment, it can compile a limited
monomorphic, first-order subset of the language to C or CUDA (via Futhark). The
compiler generates Futhark IR code that is then parsed and compiled by Futhark
to C or CUDA.

This option is accessed via the `futhark` mode:
```
$ remora futhark -f tests/basic0.remora
```

If no backend is specified via `-b`/`--backend=`, `remora futhark` simply
outputs Futhark IR. Specifying a backend will yield a target `.c` file as well
an excutable binary in the current directory:
```
$ remora futhark -f tests/basic0.remora --backend=cuda
$ ./basic0
Reading input from TTY.
Send EOF (CTRL-d) after typing all input values.
[3i32, 6i32]
```

## Docker image
`remora` can be obtained as a Docker image from the [remora-lang packages
page](https://github.com/orgs/remora-lang/packages). This image is based off of
Nvidia's [Ubuntu CUDA container](https://hub.docker.com/r/nvidia/cuda).

### Dependencies
#### Required:
- [Docker](https://www.docker.com/)

#### Optional :
- [Nvidia Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/index.html)
- An Nvidia GPU

This Nvidia stuff is required to executed binaries produced via the CUDA backend.

### Instructions
Pull the image from ghcr

```
$ docker pull ghcr.io/remora-lang/remora:latest
```

Launch the image with
```
$ docker run --rm -it ghcr.io/remora-lang/remora:latest bash
```

or with if you want to execute CUDA binaries


```
$ docker run --rm --device nvidia.com/gpu=all -it ghcr.io/remora-lang/remora:latest bash
```

Once the container launches, you'll be plopped into a shell at the root of the
`remora` repo with `remora` already built and on your path.
