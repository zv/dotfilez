;; -*- mode: emacs-lisp -*-

;; Emacs global configuration
(defconst zv-configuration-layer-directory (car dotspacemacs-configuration-layer-path)
  "zv contribution layer base directory.")

(defvar zv-whitelist '() "List of helm buffers in which to show dots.")

(define-minor-mode quick-calculate-mode
  "Local minor mode for quickly evaluating mathematical expressions inline"
  :keymap '())
