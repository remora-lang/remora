# remora-ts-mode -- a major mode for editing Remora programs

To install, put `remora-ts-mode.el` in emacs' load-path. 
To see what emacs' load-path is, do `C-h v load-path <enter>`
In your `~/.emacs` file, add

```
(load-library 'remora-ts-mode)
```

## exploring syntax tree

`M-x treesit-explore`

You can set the tree-sitter language to remora in a buffer (without
loading remora-ts-mode) by doing

```
M-: (treesit-parser-create 'remora)
```

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
