;;; imandra-mode.el --- Emacs mode for Imandra

;; Copyright (C) 2020 Imandra, Inc.

;; Author: Matt Bray <matt@imandra.ai>
;; Version: 0.1
;; URL: http://github.com/aestheticintegration/imandra-mode

(require 'lsp-mode)
(require 'merlin)
(require 'tuareg)

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

(defun imandra--opam-config-var (var)
  (with-temp-buffer
    (if (eq (call-process-shell-command
             (concat "opam config var " var) nil (current-buffer) nil)
            0)
        (replace-regexp-in-string "\n$" "" (buffer-string))
      (progn
        (message "merlin-command: opam config failed (%S)"
                 (buffer-string))
        '()))))

(defun imandra--set-merlin-configuration-function ()
  (setq-local merlin-configuration-function
              (lambda ()
                (progn
                  (let* ((bin-path (imandra--opam-config-var "bin"))
                         (lib-path (imandra--opam-config-var "lib"))
                         (stublibs-path (imandra--opam-config-var "stublibs"))
                         (env (list (concat "PATH=" bin-path)
                                    (concat "CAML_LD_LIBRARY_PATH="
                                            stublibs-path ":"
                                            (concat lib-path "/ocaml/stublibs") ":"
                                            (concat lib-path "/ocaml"))))
                         (command (list (concat bin-path "/imandra-merlin"))))
                    (list
                     (cons 'name "imandra")
                     (cons 'env env)
                     (cons 'command command)))))))

(add-hook 'imandra-mode-hook
          #'imandra--set-merlin-configuration-function)

(provide 'imandra-mode)
