;; -*- mode: emacs-lisp -*-
(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(zv)
 ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only)
 dotspacemacs-fullscreen-at-startup nil
 ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
 ;; overrides the default behavior of Emacs which recenters the point when
 ;; it reaches the top or bottom of the screen
 dotspacemacs-smooth-scrolling t
 ;; If non nil pressing 'jk' in insert state, ido or helm will activate the
 ;; evil leader.
 dotspacemacs-feature-toggle-leader-on-jk t
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '()
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository nil
)

;; Functions

;; encrypt hook ------------------------------------------------------------------
(defun zv/install/encrypt-hook ()
  (require 'epa-mail)
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
  (add-hook 'after-save-hook 'zv/encrypt-secrets))


;; org-mode --------------------------------------------------------------------
(defun zv/configure/org-mode ()
  ;; (evil-leader/set-key-for-mode 'org-mode "oa" 'org-attach  "oR" 'org-refile)
  (setq org-directory (expand-file-name "~/org/"))
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-capture-templates
        '(("t" "Todo"  entry  (file             (concat org-directory "gtd.org")) "* TODO %?\n  %i\n  %a")
          ("d" "Diary" entry (file+datetree     (concat org-directory "diary.org")))
          ("i" "Ideas"  item  (file     (concat org-directory "ideas.org")) "%?")
          ("q" "Quotes" item (file (concat org-directory "quotes.org")) "%?")))
  )


(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
 (setq evilnc-hotkey-comment-operator "gc")
)

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)
  (define-key evil-normal-state-map "\C-p" 'spacemacs/projectile-find-file)
  (setq evil-cross-lines t)
  
  (setq-default js2-global-externs '("module" "require" "buster"
                                     "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval"
                                     "clearInterval" "location" "__dirname" "console" "JSON"))

  (evil-set-initial-state 'Man-mode 'emacs)

  ; Org-Mode keybindings
  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda
    "osl" 'org-store-link)

  ; Align keybinding
  (evil-leader/set-key "al" 'align-regexp)

  
  ;; H/L should go to the first / last non blank character respectively
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-visual-state-map "H" 'evil-first-non-blank)
  (define-key evil-normal-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'evil-first-non-blank)

  ; Autocomplete 
  (global-set-key               (kbd "<backtab>") 'ac-start)

  (evil-leader/set-key "l" 'previous-buffer)

  (define-key evil-normal-state-map "g]" 'helm-etags-select)
  (define-key evil-visual-state-map "g]" 'helm-etags-select)

  (global-set-key (kbd "<XF86Calculator>") 'calc)

  (zv/configure/org-mode)
  (zv/install/encrypt-hook)

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
 '(woman-bold ((t (:foreground "DeepSkyBlue3" :weight bold))))
 '(woman-italic ((t (:foreground "lawn green" :underline t :slant oblique)))))
