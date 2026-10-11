# remora-ts-mode -- a major mode for editing Remora programs

## Installation

First of all, you need a modern emacs (30+) with treesitter support
built in.

### Install libtree-sitter-remora.so Remora grammar

Add the following to your .emacs:
```
(append '((remora "https://github.com/remora-lang/remora/utils/tree-sitter-remora"))
	treesit-language-source-alist)
```

If the variable `treesit-language-source-alist` does not exist (`C-h v treesit-language-source-alist`), then
instead add the following to your .emacs file:
```
(setq treesit-language-source-alist '((remora "https://github.com/remora-lang/remora/utils/tree-sitter-remora")))
```

Then, to build and install the tree-sitter grammar, do:
```
M-x treesit-install-language-grammar
```
and answer `remora`.

This should build `libtree-sitter-remora.so` and install it in `~/.emacs.d/treesitter/`.

To check whether the library is successfully installed, visit the
`*scratch*` buffer and type `(treesit-languate-available-p 'remora)`
and execute the line by hitting `C-j`.

### Install remora-ts-mode emacs mode

The `remora-ts-mode.el` file can be found at 
[`https://github.com/remora-lang/remora/blob/main/utils/remora-ts-mode/remora-ts-mode.el`](https://github.com/remora-lang/remora/blob/main/utils/remora-ts-mode/remora-ts-mode.el)

Copy that file to somewhere in .emacs' load-path
To see what emacs' load-path is, do `C-h v load-path`.
In your `~/.emacs` file, add

```
(load-library 'remora-ts-mode)
```

## Exploring the syntax tree

`M-x treesit-explore` will display a parse tree. Clicking on the links
in the tree will highlight the corresponding source file text.

`M-x treesit-inspect-mode` will start displaying the grammar path to
the current node in the mode line.

## gotcha's

If you have an older emacs, you might get a version mismatch with the
remora library in `~/.emacs.d/tree-sitter/`. To diagnose, visit the
`*scratch*` buffer, and type

```
(treesit-language-available-p 'remora t)
```

followed by C-j to execute the elisp expression.

To find out which ABI version your emacs supports, you can do

```
(treesit-library-abi-version)
```

To work around this issue, build (and `make install`) both emacs and
tree-sitter from their respective sources. When building emacs, do

```
./configure --with-tree-sitter --with-native-compilation
```
