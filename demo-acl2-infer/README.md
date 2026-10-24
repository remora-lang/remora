# Demo: ACL2 infers Remora's type and index applications

Remora requires a polymorphic function to be given its type and index
arguments (`t-app`, `i-app` or `@`) before it is applied. On this branch,
`remora interpret` and `remora futhark` take a flag, `--acl2-infer`. When those
applications are missing, remora sends the program to ACL2, where the book
`kestrel/remora/elaborator` tries to infer them. If ACL2 succeeds, remora
writes the elaborated program next to the original, `prog.remora` becoming
`prog-typed.remora`, and runs that instead.

`demo.sh` shows this on the three programs in this directory, and `demo.txt`
is its output.

## What you need

- Nix with flakes enabled, to build remora. You can use cabal instead; see
  step 1.
- Docker, to run ACL2. On macOS, Docker Desktop or another Docker engine.
- A C compiler, for the demo's C step. On macOS, run `xcode-select --install`
  if you don't have the Xcode command line tools.

## Setup

Run these commands from the repository root.

1. **Build remora** from this branch, and start a shell with it on the search
   path:

   ```
   nix shell .#remora-wrapped
   ```

   This builds the checked-out source. The wrapper also puts the `futhark` and
   `z3` that remora uses on remora's search path. `remora interpret --help`
   should list `--acl2-infer`.

   With cabal instead (`nix develop`, then `cabal build`, as in the top-level
   `README.md`), point the demo at that build with
   `export REMORA=$(cabal list-bin remora)`. The first `cabal build` also
   compiles Futhark, which `cabal.project` takes from git.

2. **Build the ACL2 image:**

   ```
   docker build --pull -t remora-acl2 demo-acl2-infer
   ```

   The `Dockerfile` starts from Kestrel's nightly image of ACL2 with its books
   certified, `ghcr.io/kestrelinstitute/kestrel-allcerts-java:latest`. That's
   about 2 GB to download and 10 GB on disk. Docker picks the arm64 or amd64
   variant to match your machine, and `--pull` makes it check for a newer
   nightly.

   On top of that, the `Dockerfile` adds `acl2-remora`, an ACL2 executable with
   `kestrel/remora/elaborator` already loaded, so that each call doesn't spend
   about 30 seconds loading books first. The build takes a few minutes after
   the download. It stops with a message if the base image doesn't have that
   book.

3. **Tell remora how to start ACL2:**

   ```
   export ACL2="docker run --rm -i remora-acl2"
   ```

   `$ACL2` can be any shell command that starts ACL2 reading from its standard
   input. If it isn't set, remora runs `acl2` from the search path. Either
   way, that ACL2 needs `kestrel/remora/elaborator` certified.

## Running the demo

```
demo-acl2-infer/demo.sh
```

The script copies the programs to a new temporary directory and runs remora on
them there, printing each command before its output. To keep the files, give
it a directory, as in `demo-acl2-infer/demo.sh /tmp/acl2-infer-demo`. It takes
a few seconds. The demo has five steps:

1. Without `--acl2-infer`, `reverse.remora` is an error, because it applies
   `reverse` without its type and index arguments.
2. With `--acl2-infer`, ACL2 infers them, and remora writes
   `reverse-typed.remora` and interprets that.
3. `my-last.remora` goes through the compiler, to Futhark and then C.
4. Remora prints the Futhark IR of `reverse.remora`.
5. `reduce.remora` is beyond ACL2's inference for now, so remora reports its
   own error, followed by ACL2's reason.

## Your own programs

```
remora interpret --acl2-infer -f prog.remora
remora futhark --acl2-infer --run -f prog.remora
remora futhark --acl2-infer -f prog.remora
```

- The program must be in a file, given with `-f`. Remora writes
  `prog-typed.remora` next to it, replacing it each time.
- ACL2 is called only when type checking fails because an application is
  missing. Any other error is reported as usual.
- The `-typed` file is ordinary Remora, so it runs without `--acl2-infer` or
  ACL2.

## Troubleshooting

- **`hPutChar: invalid argument (cannot encode character ...)`:** remora
  prints `∀` and `Π` in its errors, which needs a UTF-8 locale, e.g.
  `export LANG=en_US.UTF-8`.
- **`no result from ...; does that ACL2 have the book kestrel/remora/elaborator?`:**
  the command in `$ACL2` printed no result. Remora drops that command's error
  output, so run it by hand to see what it says: `sh -c "$ACL2" < /dev/null`.
  Common causes are that Docker isn't running, that the image hasn't been
  built (setup step 2), or that the ACL2 doesn't have the book.

## Limitations

This is a prototype.

- **Uniqueness is not checked.** The elaboration is the one that ACL2's
  inference finds, and nothing checks that it's the only one. The demo's
  programs have only one typing.
- **ACL2's inference is limited:**
  - It handles only one-argument calls, so write multi-argument calls
    curried, as `((f a) b)`.
  - It uses only a call's first argument.
  - It tries only the empty frame.
- **ACL2's errors are terse.** A failed inference comes back as
  `(:ERROR (FTY::INFO))`.
- **Comments inside the program are dropped** from the `-typed` file. Its
  leading comment lines are kept, so `;; output` directives carry over.
- **Imports are not supported.**

## How it works

- `src/TypeCheck.hs` reports a missing application as its own error,
  "Missing type or index application".
- On that error, `src/ACL2Infer.hs` runs `$ACL2` and sends it the program as a
  Lisp string:

  ```
  (include-book "kestrel/remora/elaborator" :dir :system)
  (remora::elaborate-program-string "<the program>" state)
  (good-bye 0)
  ```

  It then reads the elaborated program from between marker lines in ACL2's
  output.
- `src/CLI.hs` adds the flag. With it, remora writes the `-typed` file and
  runs the same command on it.
- The ACL2 side is `books/kestrel/remora/elaborator.lisp` in the ACL2
  repository.

## Files

| File | Contents |
|---|---|
| `Dockerfile` | the ACL2 image |
| `demo.sh` | the demo |
| `demo.txt` | the demo's output |
| `reverse.remora`, `my-last.remora`, `reduce.remora` | the demo's programs |
