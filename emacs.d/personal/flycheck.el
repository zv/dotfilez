;;; flymake.el --- Overview
;;; Commentary:
;; Flymake - Flyweight settings
;;; Code:

; Install a nicer cursor
; (prelude-require-package 'flymake-cursor)
; (require 'flymake-cursor)
; (setq flycheck-indication-mode 'nil)
; (setq flycheck-highlighting-mode 'symbol)

;;;
; Keybindings
;;;
(define-key evil-normal-state-map ",lo" 'flycheck-list-errors) ; [q]uickfix [o]pen
(add-hook 'flycheck-mode-hook (lambda ()
  (define-key evil-normal-state-map ",lc" 'quit-window)
))
;
