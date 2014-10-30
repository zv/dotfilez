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
  (define-key evil-normal-state-map "\C-p" 'projectile-find-file)

  (load-theme 'solarized-dark)

  (setq-default js2-global-externs '("module" "require" "buster"
                                     "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON"))
  (evil-set-initial-state 'Man-mode 'emacs)


  ;; H/L should go to the first / last non blank character respectively
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-visual-state-map "H" 'evil-first-non-blank)
  (define-key evil-normal-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'evil-first-non-blank)

  (evil-leader/set-key "l" 'switch-to-next-buffer)

  (define-key evil-normal-state-map "g]" 'helm-etags-select)
  (define-key evil-visual-state-map "g]" 'helm-etags-select)
  
  (zv/install/linum-relative)
  (zv/install/helm-ag)
  (zv/install/jade-mode)

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
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(ring-bell-function (quote ignore) t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
