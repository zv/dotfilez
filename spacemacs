;; -*- mode: emacs-lisp -*-
(setq-default
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(
                                     ;; c-c++
                                     ;; erlang-elixir
                                     javascript
                                     html
                                     zv
                                     git
                                     ))

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
 dotspacemacs-feature-toggle-leader-on-jk t
 ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
 dotspacemacs-smartparens-strict-mode nil
 ;; If non nil advises quit functions to keep server open when quitting.
 dotspacemacs-persistent-server nil
 ;; The default package repository used if no explicit repository has been
 ;; specified with an installed package.
 ;; Not used for now.
 dotspacemacs-default-package-repository "melpa"
 ;; A list of packages and/or extensions that will not be install and loaded.
 dotspacemacs-excluded-packages '(rcirc tern))

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

(defun zv/configure/abbrev-mode ()
  ;; hippie expand is dabbrev expand on steroids
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  ;; Use hippie-expand instead of dabbrev
  (global-set-key (kbd "M-/") 'hippie-expand)
  )

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
  (setenv "PATH"   (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setq exec-path (cons "/usr/local/bin" exec-path))
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

  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist `(("/home/.*" . ,temporary-file-directory)))
  (setq auto-save-default t)
  (setq auto-save-file-name-transforms `(("/home/.*" ,temporary-file-directory t)))

  ;; Default emacs modes ------------------------------------
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'epa-key-list-mode 'emacs)
  (evil-set-initial-state 'epa-key-mode 'emacs)
  (evil-set-initial-state 'epa-mail-mode 'emacs)

  ;; temporary hack to fix sc remote highlihg
  (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)

  ;; Align keybinding ---------------------------------------
  (evil-leader/set-key "al" 'align-regexp)

  ;; H/L should go to the first / last non blank character respectively
  (define-key evil-visual-state-map "L" 'evil-last-non-blank)
  (define-key evil-visual-state-map "H" 'evil-first-non-blank)
  (define-key evil-normal-state-map "L" 'evil-last-non-blank)
  (define-key evil-normal-state-map "H" 'evil-first-non-blank)

  ;; Autocomplete 
  (global-set-key (kbd "<backtab>") 'ac-start)
  (evil-leader/set-key "l" 'previous-buffer)
  (define-key evil-normal-state-map "g]" 'helm-etags-select)
  (define-key evil-visual-state-map "g]" 'helm-etags-select)

  ;; evil ---------------------------------------------------
  (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
  (setq evil-cross-lines t)
  (define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)
  ;; As I never distinguish between [[ & [{, I might as well get the
  ;; benefit of use of the easier one
  (define-key evil-motion-state-map "]" 'evil-forward-section-begin)
  (define-key evil-motion-state-map "[" 'evil-backward-section-begin)
  (define-key evil-motion-state-map "(" 'evil-previous-open-paren)
  (define-key evil-motion-state-map ")" 'evil-next-close-paren)
  ;; evil leader eval region
  (evil-leader/set-key "xe" 'eval-last-sexp)
  (evil-leader/set-key-for-mode 'evil-visual-state-map "xe" 'eval-region)

  ;; tab/window split manipulation --------------------------
  (define-key evil-normal-state-map "Q" 'evil-quit)
  ;;;; Move to next window
  (global-set-key next-buffer-key 'evil-window-next)
  (global-set-key prev-buffer-key 'evil-window-prev)
  ;;;; Transpose window forward/backwards
  (global-set-key "\C-\M-j" (lambda () (interactive) (rotate-windows 1)))
  (global-set-key "\C-\M-k" (lambda () (interactive) (rotate-windows -1)))
  ;;;; Enlarge/Shrink window
  (global-set-key "\M-h" (lambda () (interactive) (enlarge-window-by-dominant-dimension -5)))
  (global-set-key "\M-l" (lambda () (interactive) (enlarge-window-by-dominant-dimension 5)))
  ;;;; Create new window
  (global-set-key (kbd "C-M-<return>") 'tile-split-window)
  (global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

  (defun enlarge-window-by-dominant-dimension (magnitude)
    "Enlarge the current window by height if vertically split, or width otherwise"
    (cond ((window-full-width-p) (enlarge-window magnitude))
          ((window-full-height-p) (enlarge-window-horizontally magnitude))
          (t (enlarge-window (/ magnitude 2)))))

  (defun tile-split-window ()
    "If our current window width / height is greater than 1.68, split vertically"
    (interactive)
    (let* ((window-ratio (/ (window-pixel-width) (window-pixel-height)))
           (golden-ratio (/ (+ 1 (sqrt 5)) 2)))
      (if (> window-ratio golden-ratio)
          (evil-window-vsplit)
        (evil-window-split))))

  ;; swap C-j for C-x prefix keys --------------------------- 
  (global-set-key (kbd "C-j") ctl-x-map)

  ;; guide-key ----------------------------------------------
  ;;;; Set up some new guide keys
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
      (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)))

  ;; web-mode ------------------------------------------------
  (use-package web-mode
    :defer t
    :mode (("\\.phtml\\'"     . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.html\\'"      . web-mode)
           ("\\.htm\\'"       . web-mode)
           ("\\.hbs$"         . web-mode)
           ("\\.handlebars$"  . web-mode)
           ("\\.[gj]sp\\'"    . web-mode)
           ("\\.as[cp]x\\'"   . web-mode)
           ("\\.erb\\'"       . web-mode)
           ("\\.mustache\\'"  . web-mode)
           ("\\.djhtml\\'"    . web-mode))
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
  ;; eshell -------------------------------------------------
  (eval-after-load "eshell" (progn
      (require 'em-smart)
      ;; Ensure we set the path correctly
      (setq   eshell-path-env (concat "/usr/local/bin" ":" eshell-path-env)) 
      ;; Ensure eshell
      (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
      (evil-define-key 'normal eshell-mode-map (kbd "C-p") 'eshell-previous-prompt)
      (evil-define-key 'normal eshell-mode-map (kbd "C-n") 'eshell-next-prompt)
      (evil-define-key 'normal eshell-mode-map (kbd "i") 'evilshell/insert-state)

      (defun evilshell/insert-state ()
        (interactive)
        (evil-insert-state)
        (eshell-bol))

      (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
            eshell-review-quick-commands   nil
            eshell-smart-space-goes-to-end t
            eshell-where-to-jump           'begin
            eshell-buffer-maximum-lines     20000
            eshell-buffer-shorthand         t)))
  
  ;; relative line numbers ----------------------------------
  (global-linum-mode 1)
  (linum-relative-toggle)

  ;; git timemachine ----------------------------------------
  (add-hook 'git-timemachine-mode-hook 'evil-emacs-state)

  ;; persistent undo ----------------------------------------
  (global-undo-tree-mode) 

  ;; abbrev-mode --------------------------------------------
  (setq-default abbrev-mode t)
  (evil-leader/set-key
    ;; [d]efine [a] [g]lobal abbrev
    "dag" 'add-global-abbrev
    ;; [d]efine [a] [l]ocal abbrev
    "dal" 'add-mode-abbrev
    ;; [d]efine [a]n [i]nverse [g]lobal abbrev
    "daig" 'inverse-add-global-abbrev)

  ;; gnus ---------------------------------------------------
  (setq epg-user-id  "zv@nxvr.org")
  (global-set-key (kbd "<XF86Mail>") 'gnus)
  ;; helm ---------------------------------------------------
  ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
  ;; discussion of these options.
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t)

  (eval-after-load "helm"
    (lambda ()
      (define-key helm-map "\C-u" 'helm-delete-minibuffer-contents))) 
  
  
  ;; dired --------------------------------------------------
  (use-package dired
    :config
    (progn
      (define-key dired-mode-map "u" 'dired-up-directory )
      (define-key dired-mode-map "j" 'dired-next-line )
      (define-key dired-mode-map "k" 'dired-prev-line )
      (define-key dired-mode-map "f" 'dired-goto-file )
      (define-key dired-mode-map "r" 'dired-unmark )
      (define-key dired-mode-map (kbd "<f5>") 'dired-do-redisplay )))

  ;; erc  ---------------------------------------------------
  (setq erc-track-enable-keybindings t)
  (defun erc-connect ()
    "Connect to IRC."
    (interactive)
    ;; disable powerline for ERC ----------------------------
    (erc :server "irc.freenode.net" :port 6667 :nick "zv")
    (erc :server "irc.mozilla.org" :port 6667 :nick "zv")
    (erc :server "irc.oftc.net" :port 6667 :nick "zv"))

  ;; Find Files ---------------------------------------------
  (defun jump-to-file-or-dir (path)
    "We make a pretty naive check for a directory by checking the final slash"
    (if (string-match "\/$" path)
        ;; use ido-find-file-in-dir if we're binding a directory
        (ido-find-file-in-dir path)
      (find-file-existing path)))

  (setq key-file-map
        `(("feq" . "~/Development/quad/quad/")
          ("fez" . "~/.emacs.d/contrib/zv/")
          ("feg" . "~/.gnus.el")
          ("fer" . ,(concat user-emacs-directory "/" ".ercrc.el"))))

  (mapcar (lambda (binding)
            (let* ((keybinding (car binding))
                   (path (cdr binding))
                   (pathfn-sym (make-symbol (concat "zv//goto-file-special-" keybinding)))
                   (pathfn (defalias pathfn-sym `(lambda () (interactive) (jump-to-file-or-dir ,path)))))
              (evil-leader/set-key keybinding pathfn)))
          key-file-map)
  
  ;; SmartParens ---------------------------------------------
  (setq sp-autoescape-string-quote nil)

  ;; Info Mode ----------------------------------------------
  (evil-add-hjkl-bindings Info-mode-map 'emacs
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    (kbd "\M-h") 'Info-help   ; "h"
    "/" 'Info-search
    "?" 'Info-search-backward
    "\C-u" 'Info-scroll-down
    "\C-d" 'Info-scroll-up 
    "\C-t" 'Info-history-back ; "l"
    "\C-o" 'Info-history-back
    "\C-]" 'Info-follow-nearest-node
    (kbd "DEL") 'Info-scroll-down)

  ;; Configure Modeline Colors ------------------------------
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t)))

  ;; Configure Modeline Colors ------------------------------
  (mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
          '((normal . "DarkGoldenrod2")
            (insert . "chartreuse3")
            (emacs  . "SkyBlue2")
            (visual . "gray")
            (motion . "plum3")
            (lisp   . "HotPink1"))) 

  ;; eldoc --------------------------------------------------
  (add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

  (zv/configure/abbrev-mode)
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
 '(woman-italic ((t (:foreground "lawn green" :underline t :slant oblique))) t))
