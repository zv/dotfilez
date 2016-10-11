;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/dotfilez/layers/")
   dotspacemacs-configuration-layers
   '(
     auto-completion
     (c-c++ :variables c-c++-enable-clang-support t)
     ;; clojure
     elixir
     emacs-lisp
     (erlang :variables
             erlang-root-dir "/usr/local/lib/erlang"
             edts-man-root   "/usr/local/lib/erlang/erts-7.2.1/")
     ess
     git
     ;; go
     ;; haskell
     gtags
     html
     javascript
     markdown
     org
     racket
     python
     helm
     ;; haskell
     ocaml
     ;; ruby
     (rust :variables rust-enable-racer t)
     react
     typescript
     ;; sql
     (shell :variables
            shell-default-term-shell "/usr/bin/zsh")
     shell-scripts
     scheme
     syntax-checking
     systemd
     ;; dash
     zv
     )

   dotspacemacs-additional-packages '(edts nasm-mode)
   dotspacemacs-excluded-packages '(org-pomodoro spray)
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-install-packages 'used-only
   ))

(defun dotspacemacs/init ()
  "Initialization function. This function is called at the very startup of
Spacemacs initialization before layers configuration. You should not put any
user code in there besides modifying the variable values."
  ;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((agenda . 4)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         leuven
                         spacemacs-dark
                         spacemacs-light
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; dotspacemacs-command-key ":"
   ;; dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.6
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency nil
   dotspacemacs-inactive-transparency nil
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  (setq next-buffer-key (kbd "H-j")
        prev-buffer-key (kbd "H-k"))

  ;; Personal blog path
  (setq zv//blog-path (expand-file-name "~/zv.github.com/")
        ;; Undo tree file directory
        zv//undo-tree-directory "/tmp/zv/.emacs-undo")

  (setq powerline-default-separator nil)
  (setq-default exec-path-from-shell-check-startup-files nil
                evil-escape-delay 0.2
                org-directory (expand-file-name "~/Documents/")
                git-enable-github-support t))

(defun dotspacemacs/user-config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."

  ;; Where I keep my git controlled repos
  (setq-default magit-repo-dirs '("~/Development/")

                ;; Powerline default separator
                powerline-default-separator nil

                ;; Use Gtags to generate
                projectile-tags-command "gtags %s"

                ;; store all backup and autosave files in the tmp dir
                backup-directory-alist
                `((".*" . ,temporary-file-directory))
                auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t))

                ;; helm configuration
                ;; move to end or beginning of source when reaching top or bottom of source.
                helm-move-to-line-cycle-in-source     t
                ;; scroll 8 lines other window using M-<next>/M-<prior>
                helm-scroll-amount                    8
                helm-ff-file-name-history-use-recentf t

                ;; tern configuration
                tern-command '("node" "/bin/tern")

                ;; C Mode
                c-electric-mode t
                c-basic-offset  4

                ;; Javascript
                js2-basic-offset                 2
                js2-strict-missing-semi-warning  nil
                js2-include-node-externs         t
                js2-include-browser-externs      t

                ;; persistent undo
                undo-tree-auto-save-history t
                undo-tree-history-directory-alist `(("." . ,zv//undo-tree-directory))

                ;; Dont create .#FILES
                create-lockfiles nil

                ;; abbrev-mode
                abbrev-mode t)

  (unless (file-exists-p zv//undo-tree-directory)
    (make-directory zv//undo-tree-directory))

  (mapc (lambda (x) (evil-set-initial-state x 'emacs))
        '(epa-key-list-mode
          epa-key-mode
          epa-mail-mode
          Info-mode
          Man-mode
          term-mode
          calendar-mode
          racket-describe-mode)))


;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(canlock-password "9ca6d042878b195aa1f739607943049da803f282")
 '(company-selection-wrap-around t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#073642" t)
 '(global-eldoc-mode t)
 '(gud-tooltip-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-completing-read-function (quote magit-builtin-completing-read) t)
 '(magit-diff-use-overlays nil)
 '(neo-hidden-regexp-list (quote ("\\.o$" "^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$")))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("/home/zv/Documents/tasks.org" "/home/zv/Documents/agenda.org" "/home/zv/Documents/ledger.org" "/home/zv/Documents/norwars.org" "/home/zv/Documents/notes.org" "/home/zv/Documents/oldx.org" "/home/zv/Documents/quotes.org" "/home/zv/Documents/zv.org" "/home/zv/Documents/SFGOV/affordable_housing.org" "/home/zv/Documents/SFGOV/oracle_vouchers.org" "/home/zv/Documents/SFGOV/planning_commission.org")))
 '(org-default-notes-file (quote "/home/zv/Documents/notes.org"))
 '(org-html-text-markup-alist
   (quote
    ((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<span class=\"verbatim\">%s</span>"))))
 '(package-selected-packages
   (quote
    (systemd fish-mode company-shell wgrep smex ivy-hydra counsel-projectile counsel swiper ivy yapfify uuidgen tide typescript-mode py-isort org-projectile org org-download livid-mode skewer-mode simple-httpd live-py-mode link-hint git-link seq flycheck-mix eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff goto-chg undo-tree eshell-z dumb-jump diminish column-enforce-mode cargo ess-smart-equals ess-R-object-popup ess-R-data-view ctable ess julia-mode tss yaxception log4e pyvenv pytest pyenv-mode py-yapf pip-requirements hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic go-eldoc company-go go-mode nasm-mode clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode geiser racket-mode faceup caml powerline rust-mode hydra spinner markdown-mode json-snatcher json-reformat multiple-cursors js2-mode parent-mode projectile request haml-mode gitignore-mode flycheck flx magit magit-popup git-commit with-editor smartparens iedit anzu highlight f erlang eproject web-completion-data s dash-functional tern deferred pos-tip ghc haskell-mode yasnippet auto-highlight-symbol packed company dash elixir-mode pkg-info epl helm avy helm-core async auto-complete popup package-build bind-key bind-map evil solarized-theme nil xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe utop use-package tuareg toml-mode toc-org tagedit spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shm shell-pop scss-mode sass-mode ruby-end restart-emacs rainbow-delimiters racer quelpa popwin persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-plus-contrib org-bullets open-junk-file ocp-indent neotree multi-term move-text mmm-mode merlin markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger gh-md ggtags flycheck-rust flycheck-pos-tip flycheck-haskell flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help emmet-mode elisp-slime-nav edts disaster define-word company-web company-tern company-statistics company-racer company-quickhelp company-ghc company-cabal company-c-headers coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format buffer-move bracketed-paste auto-yasnippet auto-compile alchemist aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(ring-bell-function (quote ignore))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.35 :family "Sans Serif")))))
