# Remora

## Requirements
### Build dependencies
- `ghc` (tested with 9.10.3)
- `cabal`
- `z3`

### Run dependencies
#### Required:
- `z3`

#### Optional:
- `sphinx` (to build documentation)
- `futhark` (to compile to C/CUDA)
- CUDA Toolkit (to compile to CUDA)
- An Nvidia GPU (to execute binaries produced via the CUDA backend)

### Obtaining dependencies
If you use Nix, the easiest option is to enter a development shell that provides all dependencies via
```
$ nix shell
```

If flakes aren’t enabled on your system, either enable them globally or run:
```
$ nix shell --extra-experimental-features flakes
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
