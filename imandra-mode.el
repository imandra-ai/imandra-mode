;;; imandra-mode.el --- Emacs mode for Imandra

;; Copyright (C) 2020 - 2021 Imandra, Inc.

;; Author: Matt Bray <matt@imandra.ai>
;; Version: 0.1
;; URL: http://github.com/aestheticintegration/imandra-mode

(require 'tuareg)

;;;###autoload
(define-derived-mode imandra-mode tuareg-mode "Imandra")

(provide 'imandra-mode)
