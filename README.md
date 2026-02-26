# Remora

## Requirements
### Build dependencies
- `ghc` (tested with 9.10.3)
- `cabal`

### Run dependencies
#### Required:
- `z3`

#### Optional:
- `sphinx` (to build documentation)
- `futhark` (to compile to C/CUDA)
- CUDA Toolkit (to compile to CUDA)

## Building
### With Nix (recommended)
If you use Nix, the easiest option is to enter a development shell that provides
the `remora` binary:
```
$ nix develop
```

If flakes aren’t enabled on your system, either enable them globally or run:
```
$ nix develop --extra-experimental-features flakes
```

### With Cabal
If you do not use Nix, you can build with `cabal`. To do so, you must have a
working Haskell installation on your machine;
[`GHCup`](https://www.haskell.org/ghcup/) is the recommended installer.

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

## Examples
### Remora Interpreter
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

### Remora Compiler
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
an excutable binary in current directory:
```
$ remora futhark -f tests/basic0.remora --backend=cuda
$ ./basic0
Reading input from TTY.
Send EOF (CTRL-d) after typing all input values.
[3i32, 6i32]
```

#### Environment
`remora futhark --backend=cuda` depends on various CUDA libraries; see
[here](https://futhark.readthedocs.io/en/latest/man/futhark-cuda.html#environment)
for specific environment variable instructions.

If you're running on NixOS
```
$ nix develop '.#cuda'
```
should populate the environment variables appropriately and also pulls in the CUDA toolkit dependency.
