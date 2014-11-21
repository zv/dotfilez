(defvar zv-packages
  '(
    ag
    cider
    clojure-mode
    helm-ag
    jade-mode
    linum-relative
    )
  )

(defvar zv-excluded-packages '())


(defun zv/init-gitconfig-mode
  (use-package gitconfig-mode :mode "/\\.?gitconfig\\'")
  )  

(defun zv/init-gitignore-mode
  (use-package gitignore-mode :mode "/\\.gitignore\\'")
  )

(defun zv/init-neotree ()
  (use-package neotree
    :defer t
    :init
    (progn
      ;; Default neotree state is now evil-motion
      (add-to-list 'evil-motion-state-modes 'neotree-mode)
      (setq neo-show-header             nil
            neo-persist-show            nil)
      (defun neotree-up-dir (optional)
        (interactive "p")
        "(Hacky way to) Change our root to the parent directory in Neotree"
        ( search-backward "(up a dir)" )
        ( neotree-change-root ))
      (defun neotree-jump-to-parent (optional)
        "Move up to our parent directory"
        ( interactive "p" )
        ( search-backward-regexp "-\s.*/" ))
      (evil-leader/set-key "ft" 'neotree-toggle)
      ;; (define-key evil-normal-state-map ",N" 'neotree-toggle)
      (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find))
    :config
    (add-hook 'neotree-mode-hook
              (lambda ()
                ;; Jump to parent
                (define-key evil-motion-state-local-map "p" 'neotree-jump-to-parent)
                (define-key evil-motion-state-local-map "u" 'neotree-up-dir)
                (define-key evil-motion-state-local-map "C" 'neotree-change-root)
                (define-key evil-motion-state-local-map "I" 'neotree-hidden-file-toggle)
                ;; end of added keybindings
                (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-motion-state-local-map (kbd "?") 'evil-search-backward)
                (define-key evil-motion-state-local-map (kbd "a") 'neotree-stretch-toggle)
                (define-key evil-motion-state-local-map (kbd "c") 'neotree-create-node)
                (define-key evil-motion-state-local-map (kbd "d") 'neotree-delete-node)
                (define-key evil-motion-state-local-map (kbd "R") 'neotree-refresh)
                (define-key evil-motion-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
                (define-key evil-motion-state-local-map (kbd "K") 'kill-this-buffer)
                (define-key evil-motion-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-motion-state-local-map (kbd "r") 'neotree-rename-node)
                ))
    )
  )

;; Functions
;; programming mode ------------------------------------------------------------
(defun zv-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; clojure ----------------------------------------------------------------------
(defun zv-lisp-mode-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

;; no need for whitespace-mode in iteractive
(defun zv-interactive-lisp-mode-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(defun zv/init-clojure-mode ()
  (use-package clojure-mode
    :init
    (progn
      (add-hook 'clojure-mode-hook 'zv-lisp-mode-defaults))))

(defun zv/init-cider ()
  (use-package cider
    :init
    (progn
      (setq nrepl-log-messages t)
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))))

;; emacs lisp ------------------------------------------------------------
(defun zv-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (zv-start-or-switch-to 'ielm "*ielm*"))

(defun zv-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun zv-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'zv-lisp-mode-defaults)
  (turn-on-eldoc-mode)
  (rainbow-mode +1)
  (zv-conditional-emacs-lisp-checker))

;; ielm is an interactive Emacs Lisp shell
(defun zv-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'zv-interactive-lisp-mode-defaults)
  (turn-on-eldoc-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(defun zv/init-elisp ()
  (add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
  ;; Some key defs
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'zv-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

  (add-hook 'emacs-lisp-mode-hook 'zv-emacs-lisp-mode-defaults)
  (add-hook 'ielm-mode-hook 'zv-ielm-mode-defaults)

  ;; (eval-after-load "elisp-slime-nav"    '(diminish 'elisp-slime-nav-mode))
  ;; (eval-after-load "rainbow-mode"       '(diminish 'rainbow-mode))
  (spacemacs//diminish eldoc-mode " ∃")

  ;; enable elisp-slime-nav-mode
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook)) (add-hook hook 'elisp-slime-nav-mode))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
  )

;; relative linum ------------------------------------------------------------
(defun zv/init-linum-relative ()
  "Setup linum-relative"
  (use-package linum-relative
    :init
    (progn
      (global-linum-mode 1)
      (setq linum-relative-current-symbol ""))))

;; jade-mode ------------------------------------------------------------------
(defun zv/init-jade-mode ()
  "Setup jade-mode"
  (use-package jade-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
      (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode)))))

;; surround-keys ---------------------------------------------------------------
(defun zv/init-surround-keys ()
  (defun surround-word (char &optional right-char)
    "Surrounds word with char, or if right-char is supplied then
     surrounded by char and right char respectively"
    (save-excursion
      (forward-word 1)
      (if (not right-char) (insert char) (insert right-char))
      (forward-word -1)
      (insert char)))

  (defun surround-word-single-quotes () (interactive) (surround-word "'"))
  (defun surround-word-bracket () (interactive) (surround-word "[" "]"))
  (defun surround-word-parens () (interactive) (surround-word "(" ")"))
  (defun surround-word-angle-brackets () (interactive) (surround-word "<" ">"))
  (define-key evil-normal-state-map ",'" 'surround-word-single-quotes)
  (define-key evil-normal-state-map ",[" 'surround-word-bracket)
  (define-key evil-normal-state-map ",(" 'surround-word-parens)
  (define-key evil-normal-state-map ",<" 'surround-word-angle-brackets))

;; speedbar --------------------------------------------------------------------
(defun zv/init-speedbar ()
  (use-package speedbar
    :config
    (progn
      (setq
       speedbar-directory-button-trim-method  'trim
       speedbar-hide-button-brackets-flag     t
       speedbar-show-unknown-files            t
       speedbar-smart-directory-expand-flag   t
       ;; currently breaks erc
       ;; speedbar-use-images                    nil
       )


      (add-hook
       'speedbar-mode-hook
       '(lambda ()
          (linum-mode 0)))
      (custom-set-faces
       '(speedbar-file-face           ((t (:height 95 :box nil))))
       '(speedbar-directory-face      ((t (:height 95 :box nil))))
       '(speedbar-selected-face       ((t (:height 105 :box nil)))))
      )))

;; helm-ag ---------------------------------------------------------------------
(defun zv/init-helm-ag ()
  "Setup ag && helm-ag"
  (use-package ag
    :config
    (progn
      (setq
       ag-highlight-search t
       reuse-buffers       t)
      (evil-leader/set-key "ga" 'ag)))

  (use-package helm-ag
    :config
    (evil-leader/set-key
      "gr" 'ag
      "gp" 'helm-do-ag
      )))
