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

;; zv
(defun zv/install/clojure-mode ()
  (use-package clojure-mode
    :init
    (progn
      (defun zv-clojure-mode-defaults ()
        (smartparens-strict-mode +1)
        (rainbow-delimiters-mode +1))

      (setq zv-clojure-mode-hook 'zv-clojure-mode-defaults)

      (add-hook 'clojure-mode-hook (lambda () (run-hooks 'zv-clojure-mode-hook))))
    ))

;; zv
(defun zv/install/cider ()
  (use-package cider
    :init
    (progn
      (setq nrepl-log-messages t)
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    )))


(defun zv/install/linum-relative ()
  "Setup linum-relative"
  (use-package linum-relative
        :init
        (progn
          (global-linum-mode 1)
          (setq linum-relative-current-symbol ""))))

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
  (zv/install/cider)
  (zv/install/clojure-mode)
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
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
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
