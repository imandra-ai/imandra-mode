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

For `company` support, invoke `(imandra-merlin-setup-company)`
For `eldoc` support, invoke `(imandra-merlin-setup-eldoc)`

Requires https://github.com/ocaml/merlin/tree/master/emacs

Optionally requires https://github.com/ProofGeneral/opam-switch-mode for `C-c m`

Requires `ocamlmerlin-imandra` to be in PATH (can be installed via https://docs.imandra.ai/imandra-docs/notebooks/installation-simple/)

Requires `ocamlmerlin` to be installed in the switch (at the same version as the one installed in the global imandra switch, if using `ocamlmerlin-imandra` from it)

Will only works on projects that have a `dialect` stanza for iml files in `dune-project`:
```lisp
(dialect
 (name imandra)
  (implementation
    (extension iml)
    (preprocess (system "imandra-extract %{input-file}"))
    (format (system "ocamlformat %{input-file}")))
    (interface (extension "imli")))
```

## LSP

To use `lsp`:
```elisp
(require 'imandra-mode-lsp)
```
Requires https://github.com/emacs-lsp/lsp-mode

Requires `imandra-lsp` to be in PATH (can be installed via https://docs.imandra.ai/imandra-docs/notebooks/installation-simple/)

# Formatting

For autoformatting on save via `ocamlformat` (needs to be installed in the switch):

```elisp
(require 'imandra-mode-ocamlformat)
```

Requires https://github.com/ocaml-ppx/ocamlformat/blob/main/emacs/
