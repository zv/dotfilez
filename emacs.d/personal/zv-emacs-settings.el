;;; zv-emacs-settings --- s
;;; Commentary:
;;; Code:
(defvar temporary-file-directory (expand-file-name "/tmp")
  "The home of Prelude's core functionality.")

; Indent Levels
(setq js-indent-level 2)
; Autocomplete Settings
(setq company-select-next)
(setq company-idle-delay 0.35)
(scroll-bar-mode 0)

; Turn on abbreviations
(abbrev-mode 1)

(setq split-height-threshold 70)
(setq split-width-threshold 110)

;;;
; Org Mode
;;;
(add-to-list 'auto-mode-alist '("diary$" . org-mode))
