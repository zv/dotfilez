;;; zv-emacs-schema.el --- Summary
;;; Commentary:
;;;   Configure the theme, colors, etc of my Emacs setup
;;; Code:
;; make the fringe stand out from the background
(setq x-underline-at-descent-line t)

; Default Font
(set-frame-font "Inconsolata-11")
; (set-frame-font "Terminus-14")



;;;
; Solarized
;;;
(prelude-require-package 'solarized-theme)
(defvar solarized-color 'dark)

(defun setup-solarized-dark ()
    (load-theme 'solarized-dark t)
    ; (setq powerline-color1 "#073642")
    ; (setq powerline-color2 "#002b36")
    ;(set-face-background 'mode-line "#2aa198")
    ;(set-face-foreground 'mode-line "#fdf6e3")
  )
(if (equal solarized-color 'dark)
    (setup-solarized-dark)
(load-theme 'solarized-light t))

(setq linum-relative-current-symbol "")



;;;
;; Relative Line Numbers
;;;
(prelude-require-package 'linum-relative)
(require 'linum-relative)
(global-linum-mode 1)

;;; end
