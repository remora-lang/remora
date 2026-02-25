# Remora

## Requirements
### Build dependencies
- `ghc` (tested with 9.10.3)
- `cabal`

### Run dependencies
#### Required:
- `z3`

#### Optional:
- `sphinx` (build documentation)
- `futhark` (compile to C/CUDA)
- CUDA Toolkit (compile to CUDA)

## Building
### With Nix (recommended)
If you use Nix, the easiest option is to enter a development shell that provides
the `remora` binary:

```bash
nix shell
```

If flakes aren’t enabled on your system, either enable them globally or run:
```bash
nix shell --extra-experimental-features flakes
```

### With Cabal
If you do not use Nix, you can build with `cabal`. To do so, you must have a
working Haskell installation on your machine;
[`GHCup`](https://www.haskell.org/ghcup/) is the recommended installer.

To install `remora` to your local binary directory (defaults to `~/.local/bin`):

```bash
cabal install
```

If you’d rather build without installing:
```bash
cabal build
cabal exec -- remora interpret -e "(+ 1 2)"
```

## Running
List available modes and global options:
```bash
remora --help
```

You can also ask for help for a specific mode. For example:
```bash
remora interpret --help
```
