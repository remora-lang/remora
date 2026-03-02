# Remora
`remora` is a compiler and interpreter for the Remora programming language. It
can interpret programs directly or compile them to C or CUDA via
[Futhark](https://futhark-lang.org).

## Getting Started
### With [Nix](https://nixos.org)
To get a shell with the `remora` binary, just run:
```
$ nix shell
```

### With [Docker](https://www.docker.com/)
Pull and run the image:
```
$ docker pull ghcr.io/remora-lang/remora:latest
$ docker run --rm -it ghcr.io/remora-lang/remora:latest bash
```

This will place you into a shell at the root of this repo with the `remora`
binary available on your `PATH`.

To run CUDA-compiled binaries inside the container, you'll need:
- [Nvidia Container
  Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/index.html)
- An Nvidia GPU (to execute binaries produced via the CUDA backend)

Once you have the Nvidia Container Toolkit, pass through your GPU to the container when you run it:
```
$ docker run --rm --device nvidia.com/gpu=all -it ghcr.io/remora-lang/remora:latest bash
```

## Building
### Build dependencies
#### Required:
- [`ghc`](https://www.haskell.org/ghc/) (tested with 9.10.3)
- [`cabal`](https://www.haskell.org/cabal/download.html)
- [`z3`](https://github.com/Z3Prover/z3)

#### With Nix
Enter a development shell with all dependencies:
```
$ nix develop
```

#### With the Docker image
The Docker image has all dependencies.

### How to Build
Build `remora` via `cabal`:
```
$ cabal build
```

To install the `remora` binary to a local directory (configured in `cabal`, but it'll tell you where it installs to):
```
$ cabal install
```

You can also execute directly via `cabal`:
```
$ cabal exec -- remora interpret -e "(+ 1 2)"
```

## Running
### Run dependencies
#### Required:
- [`z3`](https://github.com/Z3Prover/z3)

#### Optional:
- [`futhark`](https://futhark-lang.org) (to compile to C/CUDA)
- [CUDA Toolkit](https://developer.nvidia.com/cuda/toolkit) (to compile to CUDA)
- An Nvidia GPU (to execute binaries produced via the CUDA backend)

> **Note:** You may need to set environment variables needed for CUDA
> compilation. See Futhark's
> [docs](https://futhark.readthedocs.io/en/latest/man/futhark-cuda.html#environment)
> for instructions on how to set them. If you're using the Docker image, this
> should be unnecessary. If you're using `nix shell` or if you're using `nix
> develop` on a non-NixOS system, it will likely be necessary.

### Usage
```
$ remora --help
$ remora <mode> --help
```

### Interpreter
Run a REPL:
```
$ remora repl
>> (+ 1 2)
3
```

Interpret a file:
```
$ remora interpret -f tests/basic0.remora
[3 6]
```

### Compiler
The compiler targets C and CUDA via Futhark. It's currently in early stages and
supports a monomorphic, first-order subset of the language.

Without a backend specified, `remora futhark` outputs Futhark IR:
```
$ remora futhark -f tests/basic0.remora
```

With a backend, it produces a `.c` file and an executable binary in the current directory:
```
$ remora futhark -f tests/basic0.remora --backend=cuda
$ ./basic0
Reading input from TTY.
Send EOF (CTRL-d) after typing all input values.
[3i32, 6i32]
```

## Examples

There are various basic test files in the `tests` directory. Most of them are
self-explanatory. All tests should run with `remora interpret`, but some may not
with `remora futhark` due to the current restrictions on lowering (namely a lack
of monomorphization and defunctionalization).

`tests/accel.remora` computes the acceleration of a particle on another. This is
the core computation from the `examples/nbody.remora` program, which computes
accelerations (due to gravity) between particles in space.

Since the compiler doesn't have a monomorphization pass at the moment,
`nbody.remora` can't currently be compiled. However, `accel.remora` is
monomorphic and can be compiled and run on a GPU:

```
$ remora futhark -f tests/accel.remora --backend=cuda
$ echo "" | ./accel
[0.074032f32, 0.001346f32, 0.033651f32]
```
