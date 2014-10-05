;;; zv-emacs-schema.el --- Summary
;;; Commentary:
;;;   Configure the theme, colors, etc of my Emacs setup
;;; Code:

;;;
; Solarized
;;;
(prelude-require-package 'solarized-theme)

;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Use less bolding
(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)
;; Use less colors for indicators such as git:gutter, flycheck and similar.
(setq solarized-emphasize-indicators nil)

(setq x-underline-at-descent-line t)
(load-theme 'solarized-dark t)
; (load-theme 'solarized-light t)
; Default Font
(set-frame-font "Inconsolata-11")
; (set-frame-font "Terminus-14")



;;;
; Powerline
;;;
(require 'powerline)
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq linum-relative-current-symbol "")
(custom-set-faces
 '(mode-line ((t (:height 90 :foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:height 90 :foreground "#f9f9f9" :background "#666666" :box nil)))))

;;;
;; Relative Line Numbers
;;;
(prelude-require-package 'linum-relative)
(require 'linum-relative)
(global-linum-mode 1)

;;; end