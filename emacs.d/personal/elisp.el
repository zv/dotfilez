;;; elisp.el --- Elisp Settings
;;; Commentary:
;   Enable some useful elisp settings
;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "M-e") 'eval-last-sexp)
            (define-key evil-visual-state-map (kbd "M-e") 'eval-region)
            )
)
