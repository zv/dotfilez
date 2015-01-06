;; -*- mode: emacs-lisp -*-
(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(
                                     ;; c-c++
                                     erlang-elixir
                                     javascript
                                     html
                                     zv
                                     git
                                     )
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository "melpa"
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(rcirc tern))

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
 ;; If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).
 dotspacemacs-fullscreen-at-startup nil
 ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
 ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
 dotspacemacs-maximized-at-startup nil
 ;; If non nil unicode symbols are displayed in the mode line (e.g. for lighters)
 dotspacemacs-mode-line-unicode-symbols t
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

(setq-default
 ;; Org Mode
 org-directory (expand-file-name "~/org")
 ;; Directory to use for ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ;; IRC commands to ignore in tracking
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 vc-follow-symlinks         t
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
  (autoload 'esup "esup" "Emacs Start Up Profiler." nil)
  (setenv "PATH"   (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setq exec-path  (cons "/usr/local/bin" exec-path))

  (setq evilnc-hotkey-comment-operator "gc")
  ;; Customize which keys we will use to move forward and backward 
  (setq next-buffer-key "\M-j")
  (setq prev-buffer-key "\M-k")

  (defun add-semicolon-to-end-of-line ()
    "Unsurprisingly, this adds a semicolon to the end of the line"
    (interactive)
    (save-excursion (end-of-line) (insert ";"))))

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; Basic configuration
  (setq powerline-default-separator nil)
  ;; don't use tabs to indent
  (setq-default indent-tabs-mode nil)   

  ;; Load our skeleton files
  (load (concat user-emacs-directory "skeleton_defs.el"))

  ;; guide-key ----------------------------------------------
  ;; Set up some new guide keys
  (add-to-list 'guide-key/guide-key-sequence "C-j")
  (add-to-list 'guide-key/guide-key-sequence "<f1>")
  (add-to-list 'guide-key/guide-key-sequence "<f9>")

  ;; calculator ---------------------------------------------
  (use-package calc
    :config
    (progn
      (global-set-key (kbd "<XF86Calculator>") 'calc)
      ;; These keys are typically bound to `kill line' which I rarely use.
      (define-key calc-mode-map next-buffer-key 'evil-window-prev)))

  ;; neotree ------------------------------------------------
  (use-package neotree
    :config
    (progn
      (setq neo-theme 'ascii
            neo-show-hidden-files nil)
      (define-key neotree-mode-map "p" 'neotree-jump-to-parent)
      (define-key neotree-mode-map "u" 'neotree-up-dir)
      (define-key neotree-mode-map "C" 'neotree-change-root)
      (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)))

    ;; web-mode ------------------------------------------------
  (use-package web-mode
    :defer t
    :mode (("\\.hbs$" . web-mode))
    :config
    (progn
      ;; CSS colorization 
      (setq web-mode-enable-css-colorization t
            web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2)

      (defun zv-web-mode-hook () (turn-off-smartparens-mode))
      (add-hook 'web-mode-hook 'zv-web-mode-hook)

      (evil-define-key 'motion web-mode-map "[" 'web-mode-attribute-beginning)
      (evil-define-key 'motion web-mode-map "]" 'web-mode-attribute-next)
      (evil-define-key 'motion web-mode-map "}" 'web-mode-element-next)
      (evil-define-key 'motion web-mode-map "{" 'web-mode-element-previous)
      (evil-define-key 'motion web-mode-map ")" 'web-mode-block-next)
      (evil-define-key 'motion web-mode-map "(" 'web-mode-block-previous)
      (evil-define-key 'motion web-mode-map "^" 'web-mode-element-parent)
      (evil-define-key 'normal web-mode-map "za" 'web-mode-element-children-fold-or-unfold)
      (define-key web-mode-map "\C-backspace" 'web-mode-element-kill)

      (evil-leader/set-key-for-mode 'web-mode
        "mas" 'web-mode-tag-attributes-sort
        "mts" 'web-mode-tag-sel
        "mes" 'web-mode-element-select
        "mer" 'web-mode-element-rename
        "meb" 'web-mode-element-beginning
        "mee" 'web-mode-element-end
        "mce" 'web-mode-element-close
        "msi" 'web-mode-element-content-select
        "mse" 'web-mode-element-select)))

  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
 
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
  (global-set-key (kbd "<XF86Mail>") 'gnus)
  (evil-leader/set-key "ag" 'gnus)

  ;; helm ---------------------------------------------------
  (eval-after-load "helm"
    (lambda ()
      (define-key helm-map "\C-u" 'helm-delete-minibuffer-contents))) 

  ;; elisp   --------------------------------------------------
  (add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.35 :family "Sans Serif"))))
 '(speedbar-directory-face ((t (:height 95 :box nil))))
 '(speedbar-file-face ((t (:height 95 :box nil))))
 '(speedbar-selected-face ((t (:height 105 :box nil))))
 '(woman-bold ((t (:foreground "DeepSkyBlue3" :weight bold))) t)
 '(woman-italic ((t (:foreground "DarkGreen" :underline t :slant oblique))) t))
