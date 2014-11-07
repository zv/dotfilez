;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Variables

(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '()
 ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only)
 dotspacemacs-fullscreen-at-startup nil
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '()
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository nil
)
;; Functions
;; programming mode ------------------------------------------------------------
;; TODO: Doesn't work!
(defun zv-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun zv/configure/prog-mode ()
  ;; (add-hook 'prog-mode-hook (lambda () (run-hooks 'zv-font-lock-comment-annotations)))
  )

;; clojure ----------------------------------------------------------------------

(defun zv-lisp-mode-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

;; no need for whitespace-mode in iteractive
(defun zv-interactive-lisp-mode-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))


(defun zv/install/clojure-mode ()
  (use-package clojure-mode
    :init
    (progn
      (add-hook 'clojure-mode-hook 'zv-lisp-mode-defaults))))


(defun zv/install/cider ()
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

(defun zv/configure/elisp ()
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
(defun zv/install/linum-relative ()
  "Setup linum-relative"
  (use-package linum-relative
        :init
        (progn
          (global-linum-mode 1)
          (setq linum-relative-current-symbol ""))))

;; jade-mode ------------------------------------------------------------------
(defun zv/install/jade-mode ()
  "Setup jade-mode"
  (use-package jade-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
      (add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode)))))

(defun zv/install/helm-ag ()
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

;; encrypt hook ------------------------------------------------------------------
(defun zv/install/encrypt-hook ()
  "Install a hook to encrypt some files after saving"
  (defun zv/encrypt-secrets ()
    "Encrypt this file if it is in one of our `dirs-to-encrypt'"
    (let* ((zv-dotfiles (expand-file-name "~/Development/dotfilez/"))
           (dirs-to-encrypt `(,(expand-file-name "~/.gnupg")
                              ,(expand-file-name (concat org-directory "/"))
                              ,(concat zv-dotfiles "gnupg/")
                              ,(concat zv-dotfiles "ssh/")
                              ,(expand-file-name "~/.ssh/")))
           (recipient (epg-list-keys (epg-make-context epa-protocol) "<zv@nxvr.org>" 'public)))
      (when (member (file-name-directory (buffer-file-name)) dirs-to-encrypt)
        (epa-encrypt-file (buffer-file-name) recipient))))

  (add-hook 'after-save-hook 'zv/encrypt-secrets)
  )

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."

)

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (require 'epa-mail)
  (define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)
  (define-key evil-normal-state-map "\C-p" 'spacemacs/projectile-find-file)
  (setq evil-cross-lines t)
  
  (load-theme 'solarized-dark)

  (setq-default
   undo-tree-auto-save-history t
   undo-tree-history-directory-alist `(("." . "/tmp/undo-tree-history"))
  )

  (setq-default js2-global-externs '("module" "require" "buster"
                                     "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON"))

  (evil-set-initial-state 'Man-mode 'emacs)

  (define-key evil-normal-state-map ",a" 'align-regexp)

  ;; H/L should go to the first / last non blank character respectively
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-visual-state-map "H" 'evil-first-non-blank)
  (define-key evil-normal-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'evil-first-non-blank)

  (evil-leader/set-key "l" 'previous-buffer)

  (define-key evil-normal-state-map "g]" 'helm-etags-select)
  (define-key evil-visual-state-map "g]" 'helm-etags-select)
  
  (zv/install/linum-relative)
  (zv/install/helm-ag)
  (zv/install/jade-mode)
  (zv/install/clojure-mode)
  (zv/install/cider)
  (zv/configure/elisp)

  (zv/install/encrypt-hook)
  ;; Install our git related modes
  (use-package gitconfig-mode :mode "/\\.?gitconfig\\'")
  (use-package gitignore-mode :mode "/\\.gitignore\\'")

  ;; Escape should escape things
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state)

  (setq exec-path (cons "/usr/local/bin" exec-path))
  (setq powerline-default-separator nil)
  )

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(edts-man-root "/home/zv/.emacs.d/edts/doc/17.3")
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(js2-basic-offset 2 t)
 '(js2-bounce-indent-p t t)
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
