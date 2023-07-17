# Basic Usage

```elisp
(require 'imandra-mode)
(add-to-list 'auto-mode-alist '("\\.iml[i]?\\'" . imandra-mode))
```

Requires https://github.com/ocaml/tuareg

# Autocompletion, jump-to-def, type info etc

You can either use `merlin` or `lsp`

## Merlin

To use `merlin`:
```elisp
(require 'imandra-mode-merlin)
```

Requires https://github.com/ocaml/merlin/tree/master/emacs
Optionally requires https://github.com/ProofGeneral/opam-switch-mode for `C-c m`

## LSP

To use `lsp`:
```elisp
(require 'imandra-mode-lsp)
```
Requires https://github.com/emacs-lsp/lsp-mode

# Formatting

For autoformatting on save via `ocamlformat`:

```elisp
(require 'imandra-mode-ocamlformat)
```

Requires https://github.com/ocaml-ppx/ocamlformat/blob/main/emacs/
