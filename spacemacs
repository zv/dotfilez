;; -*- mode: emacs-lisp -*-
(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()

 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(
                                     ;; Too slow on startup
                                     ;; c-c++
                                     ;; Causing problems with heml
                                     ;; erlang-elixir
                                     git
                                     html
                                     javascript
                                     company-mode
                                     zv
                                     )

 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(aggressive-indent
                                  google-translate
                                  ;; We use our own here
                                  evil-org
                                  ledger-mode
                                  monokai
                                  rcirc
                                  rcirc-color
                                  zenburn-theme))



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
 ;; A value from the range (0..100), in increasing opacity, which describes the
 ;; transparency level of a frame when it's active or selected. Transparency can
 ;; be toggled through `toggle-transparency'.
 dotspacemacs-active-transparency 90
 ;; A value from the range (0..100), in increasing opacity, which describes the
 ;; transparency level of a frame when it's inactive or deselected. Transparency
 ;; can be toggled through `toggle-transparency'.
 dotspacemacs-inactive-transparency 90
 ;; If non nil unicode symbols are displayed in the mode line (e.g. for lighters)
 dotspacemacs-mode-line-unicode-symbols t
 )
(setq-default
 ;; Org Mode
 org-directory (expand-file-name "~/org")
 ;; ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 vc-follow-symlinks         t
 ;; C Mode
 c-electric-mode t
 c-basic-offset  4
 ;; Javascript
 js2-global-externs '("module" "assert" "buster" "clearInterval" "clearTimeout" "console"
                      "__dirname" "JSON" "location" "refute" "require" "setInterval" "setTimeout"
                      "sinon" "Quad" "quad" "DS")
 js2-basic-offset                 2
 js2-include-node-externs         t
 js2-include-browser-externs      t)



(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
  (spacemacs/set-font "Source Code Pro" 12)
  (setenv "PATH"  (concat "/usr/local/bin" ":" (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  ;; Configure load path
  ;; (add-to-list 'load-path "~/.emacs.d/contrib/zv/extensions/org/lisp")
  (add-to-list 'load-path "~/.emacs.d/contrib/zv/extensions/org/contrib/lisp" t)

  ;; (setq evilnc-hotkey-comment-operator "gc")
  ;; Customize which keys we will use to move forward and backward 
  (setq next-buffer-key "\M-j"
        prev-buffer-key "\M-k"))

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; don't use tabs to indent
  ;; (setq-default indent-tabs-mode nil)   
  ;; Powerline default separator

  (setq powerline-default-separator nil)

  ;; Set the non-normal prefix to Hyper key
  (setq evil-leader/non-normal-prefix "H-") 

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
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; abbrev-mode --------------------------------------------
  (setq-default abbrev-mode t)
  
  ;; git timemachine ----------------------------------------
  (add-hook 'git-timemachine-mode-hook 'evil-emacs-state)

  ;; helm ---------------------------------------------------
  (eval-after-load "helm"
    '(progn
      (define-key helm-map "\C-u" 'helm-delete-minibuffer-contents))) 

  ;; SmartParens ---------------------------------------------
  (setq sp-autoescape-string-quote nil))


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
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
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
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.35 :family "Sans Serif"))))
 '(woman-bold ((t (:foreground "DeepSkyBlue3" :weight bold))))
 '(woman-italic ((t (:foreground "DarkGreen" :underline t :slant oblique)))))
