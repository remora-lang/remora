# tree-sitter grammar for Remora programming language

## To make changes to grammar

- get into nix shell, `nix develop`
- edit `grammar.js`
- `tree-sitter generate` to compile grammar
- add tests under `test/corpus`
- `tree-sitter test`
- `tree-sitter build` to build `remora.so`

- to support `remora-ts-mode` mode in emacs, copy `remora.so` to 
`~/.emacs.d/tree-sitter/libtree-sitter-remora.so`

