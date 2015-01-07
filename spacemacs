;; -*- mode: emacs-lisp -*-
(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(
                                     c-c++
                                     erlang-elixir
                                     git
                                     html
                                     javascript
                                     zv
                                     )
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository 'melpa
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(tern))

(setq-default
 ;; Specify the startup banner. If the value is an integer then the
 ;; banner with the corresponding index is used, if the value is `random'
 ;; then the banner is chosen randomly among the available banners, if
 ;; the value is nil then no banner is displayed.
 dotspacemacs-startup-banner nil
 ;; Default theme applied at startup
 dotspacemacs-default-theme 'leuven
 ;; The leader key
 dotspacemacs-leader-key "SPC"
 ;; Major mode leader key is a shortcut key which is the equivalent of
 ;; pressing `<leader> m`
 dotspacemacs-major-mode-leader-key ","
 ;; The command key used for Evil commands (ex-commands) and
 ;; Emacs commands (M-x).
 ;; By default the command key is `:' so ex-commands are executed like in Vim
 ;; with `:' and Emacs commands are executed with `<leader> :'.
 dotspacemacs-command-key ":"
 ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
 ;; the commands bound to the current keystrokes.
 dotspacemacs-guide-key-delay .6
 ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
 ;; overrides the default behavior of Emacs which recenters the point when
 ;; it reaches the top or bottom of the screen
 dotspacemacs-smooth-scrolling t
 ;; If non nil pressing 'jk' in insert state, ido or helm will activate the
 ;; evil leader.
 dotspacemacs-feature-toggle-leader-on-jk nil
 ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
 dotspacemacs-smartparens-strict-mode nil
 ;; If non nil advises quit functions to keep server open when quitting.
 dotspacemacs-persistent-server nil
 )

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
  (setenv "PATH"   (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setq exec-path  (cons "/usr/local/bin" exec-path))

  ;; (setq evilnc-hotkey-comment-operator "gc")
  ;; Customize which keys we will use to move forward and backward 
  (setq next-buffer-key "\M-j")
  (setq prev-buffer-key "\M-k")
  )

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; Load our skeleton files
  (load (concat user-emacs-directory "skeleton_defs.el"))

  ;; guide-key ----------------------------------------------
  ;; Set up some new guide keys
  (add-to-list 'guide-key/guide-key-sequence "C-j")
  (add-to-list 'guide-key/guide-key-sequence "<f1>")
  (add-to-list 'guide-key/guide-key-sequence "<f9>")

  ;; erc
  (setq erc-track-enable-keybindings t)

  ;; neotree
  (setq neo-theme 'ascii
        neo-show-hidden-files nil)

  ;; web-mode ------------------------------------------------
  (setq web-mode-enable-css-colorization t
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (defun zv-web-mode-hook () (turn-off-smartparens-mode))
  (add-hook 'web-mode-hook 'zv-web-mode-hook)

  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))

  ;; helm ---------------------------------------------------
  ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed discussion of these options.
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t)

  ;; relative line numbers ----------------------------------
  (global-linum-mode 1)
  (linum-relative-toggle)

  ;; persistent undo ----------------------------------------
  (global-undo-tree-mode) 

  ;; abbrev-mode --------------------------------------------
  (setq-default abbrev-mode t)
  (zv/configure/abbrev-mode)

  ;; git timemachine ----------------------------------------
  (add-hook 'git-timemachine-mode-hook 'evil-emacs-state)

  ;; gnus ---------------------------------------------------
  (setq epg-user-id  "zv@nxvr.org")

  ;; helm ---------------------------------------------------
  (eval-after-load "helm"
    (lambda ()
      (define-key helm-map "\C-u" 'helm-delete-minibuffer-contents))) 

  ;; elisp   --------------------------------------------------
  (add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

  ;; SmartParens ---------------------------------------------
  (setq sp-autoescape-string-quote nil)

  (zv/install/encrypt-hook))

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
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(edts-man-root "/home/zv/.emacs.d/edts/doc/17.3")
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(org-agenda-files
   (quote
    ("~/org/gtd.org" "~/org/todo.org" "~/org/agenda.org" "~/org/quad.org")))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(send-mail-function (quote smtpmail-send-it))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.35 :family "Sans Serif"))))
 '(woman-bold ((t (:foreground "DeepSkyBlue3" :weight bold))) t)
 '(woman-italic ((t (:foreground "DarkGreen" :underline t :slant oblique))) t))
