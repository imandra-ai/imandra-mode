(require 'imandra-mode)
(require 'ocamlformat)

(defun imandra--ocamlformat-before-save ()
  (interactive)
  (when (eq major-mode 'imandra-mode) (ocamlformat)))

(add-hook 'before-save-hook #'imandra--ocamlformat-before-save)

(provide 'imandra-mode-ocamlformat)
