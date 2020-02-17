;;; imandra-mode.el --- Emacs mode for Imandra

;; Copyright (C) 2020 Imandra, Inc.

;; Author: Matt Bray <matt@imandra.ai>
;; Version: 0.1
;; URL: http://github.com/aestheticintegration/imandra-mode

(require 'tuareg)
(require 'lsp-mode)

;;;###autoload
(define-derived-mode imandra-mode tuareg-mode "Imandra")

(defgroup imandra-lsp nil
  "LSP support for Imandra"
  :group 'lsp-mode)

(defcustom imandra-lsp-command
  "imandra-lsp"
  "Path to the Imandra language server binary"
  :group 'imandra-lsp
  :risky t
  :type '(choice
          (file :tag "Filename (default is \"imandra-lsp\")")
          (const :tag "Use current opam switch" opam)))

(defun imandra--lsp-command ()
  "Generate LSP startup command for the Imandra Language Server."
  (cond
   ((stringp imandra-lsp-command) imandra-lsp-command)
   ((equal imandra-lsp-command 'opam) (list "opam" "exec" "--" "imandra-lsp"))))

(add-to-list 'lsp-language-id-configuration '(imandra-mode . "imandra"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'imandra--lsp-command)
  :major-modes '(imandra-mode)
  :server-id 'imandra-lsp))

(add-hook 'imandra-mode-hook #'lsp)

(provide 'imandra-mode)
