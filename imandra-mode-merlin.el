(require 'merlin)
(require 'imandra-mode)

(defun imandra--find-merlin ()
  (with-temp-buffer
    (if (eq (call-process-shell-command "opam var bin" nil (current-buffer) nil) 0)
        (let ((bin-path (replace-regexp-in-string "\n$" "" (buffer-string))))
          (concat bin-path "/ocamlmerlin"))
      (car (ignore-errors (process-lines "which" "ocamlmerlin"))))))

(defgroup imandra-merlin nil
  "merlin support for Imandra"
  :group 'merlin-mode)

(defcustom imandra-merlin-command
  'auto
  "Path to the ocamlmerlin binary"
  :group 'imandra-merlin
  :risky t
  :type '(choice
          (file :tag "Filename")
          (const :tag "Use current opam switch" opam)
          (const :tag "Automatically find the most relevant ocamlmerlin binary (default)" auto)))

(defun imandra--merlin-command ()
  (cond
   ((stringp imandra-merlin-command) imandra-merlin-command)
   ((equal imandra-merlin-command 'opam) 'opam)
   ((equal imandra-merlin-command 'auto) (imandra--find-merlin))))

(defun imandra--set-merlin-configuration-function ()
  (setq-local merlin-configuration-function
              (lambda ()
                (let* ((flags "-addsuffix .iml:.imli -assocsuffix .iml:imandra")
                       (is-iml-buffer (when buffer-file-name (string-match "\\.iml\\'" buffer-file-name)))
                       (flags (if is-iml-buffer (concat "-open Imandra_prelude " flags) flags))
                       (command (imandra--merlin-command)))
                  (list
                   (cons 'name "imandra")
                   (cons 'flags flags)
                   (cons 'command command))))))

(defun imandra--setup-eldoc ()
  (setq-local eldoc-echo-area-use-multiline-p nil)
  (merlin-eldoc-setup))

(defun imandra-merlin-setup-eldoc ()
  (require 'imandra-eldoc)
  (add-hook 'imandra-mode-hook #'imandra--setup-eldoc))

(defun imandra-merlin-setup-company ()
  (require 'merlin-company)
  (add-hook 'imandra-mode-hook #'company-mode t))

(defun imandra--merlin-restart ()
  (interactive)
  (require 'opam-switch-mode)
  (call-interactively #'opam-switch-set-switch)
  (merlin-stop-server))

(define-key imandra-mode-map (kbd "C-c m") #'imandra--merlin-restart)
(define-key imandra-mode-map (kbd "M-.") #'merlin-locate)
(define-key imandra-mode-map (kbd "M-,") #'merlin-pop-stack)

(add-hook 'imandra-mode-hook #'merlin-mode t)
(add-hook 'imandra-mode-hook #'imandra--set-merlin-configuration-function)

(provide 'imandra-mode-merlin)
