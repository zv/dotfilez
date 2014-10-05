;;; prelude-key-chord.el --- Key chord setup
;;; Commentary:
;   Configure NeoTree
;;; Code:
;; Neotree settings
(prelude-require-package 'neotree)
(require 'neotree)

(define-key evil-normal-state-map ",N" 'neotree-toggle)      ; ,N to open neotree
(define-key evil-normal-state-map (kbd "C-\\") 'neotree-find) ; <C-\> brings us to our current file

(setq-default neo-show-header nil)

(defun neotree-up-dir (optional)
  "(Hacky way to) Change our root to the parent directory in Neotree"
  ( interactive "p" )
  ( evil-goto-first-line )
  ( evil-next-line )
  ( neotree-change-root )
)

(defun neotree-jump-to-parent (optional)
  "Move up to our parent directory"
  ( interactive "p" )
  ( search-backward-regexp "-\s.*/" )
)

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map "I" 'neotree-hidden-file-toggle)
            (define-key evil-normal-state-local-map "C" 'neotree-change-root)
            (define-key evil-normal-state-local-map "R" 'neotree-refresh)
            (define-key evil-normal-state-local-map "u" 'neotree-up-dir)
            (define-key evil-normal-state-local-map "p" 'neotree-jump-to-parent)
            )
)

;
