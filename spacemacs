;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/dotfilez/layers/")
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     (c-c++ :variables c-c++-enable-clang-support t)
     ;; clojure
     elixir
     emacs-lisp
     (erlang :variables
             erlang-root-dir "/usr/local/lib/erlang/erts-7.1"
             edts-man-root   "/usr/local/lib/erlang/erts-7.1")
     git
     ;; go
     haskell
     gtags
     html
     javascript
     markdown
     org
     ;; haskell
     ocaml
     ;; ruby
     rust
     ;; sql
     (shell :variables shell-default-term-shell "/usr/bin/zsh")
     syntax-checking
     ;; dash
     zv)

   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(org-pomodoro spray yasnippet)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(leuven
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
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
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
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
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  (setq next-buffer-key (kbd "H-j")
        prev-buffer-key (kbd "H-k"))

  ;; Personal blog path
  (setq zv//blog-path (expand-file-name "~/zv.github.com/")
        ;; Undo tree file directory
        zv//undo-tree-directory "/tmp/.emacs-undo")

  (setq-default
   exec-path-from-shell-check-startup-files nil
   evil-escape-delay 0.2
   org-directory (expand-file-name "~/Documents/")
   git-enable-github-support t))

(defun dotspacemacs/user-config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  ;; Where I keep my git controlled repos
  (setq magit-repo-dirs '("~/Development/"))

  ;; Powerline default separator
  (setq powerline-default-separator nil)

  ;; Use Gtags to generate
  (setq projectile-tags-command "gtags %s")

  ;; Set our gpg program
  (setq epg-gpg-program "/usr/bin/gpg2"
        epg-user-id "0xF6F2D0445DC172F8")

  ;; Racer
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t)

  (setq racer-cmd "/home/zv/Development/racer/target/release/racer")
  (setq racer-rust-src-path "/home/zv/Development/rust/src/")

  ;; neotree
  (setq neo-theme 'ascii
        neo-show-hidden-files nil
        neo-vc-integration nil)

  ;; smartparens
  (eval-after-load 'smartparens
    '(progn
       (sp-pair "(" nil :actions :rem)
       (sp-pair "<" nil :actions :rem)
       (sp-pair "[" nil :actions :rem)
       (sp-pair "'" nil :actions :rem)
       (sp-pair "\"" nil :actions :rem)))

  ;; Mode setting
  (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))


  ;; As I never distinguish between [[ & [{, I might as well get the
  ;; benefit of use of the easier one
  (define-key evil-normal-state-map "]" 'evil-forward-paragraph)
  (define-key evil-normal-state-map "[" 'evil-backward-paragraph)
  (define-key evil-normal-state-map "}" 'evil-forward-section-begin)
  (define-key evil-normal-state-map "{" 'evil-backward-section-begin)
  (define-key evil-normal-state-map (kbd "M-]") 'evil-forward-section-end)
  (define-key evil-normal-state-map (kbd "M-[") 'evil-backward-section-end)
  ;;(define-key evil-motion-state-map "(" 'evil-previous-open-paren)
  ;;(define-key evil-motion-state-map ")" 'evil-next-close-paren)
  (define-key evil-normal-state-map (kbd "M-{") 'evil-previous-open-brace)
  (define-key evil-normal-state-map (kbd "M-}") 'evil-next-close-brace)


  ;; relative line numbers ----------------------------------
  (linum-relative-toggle)

  ;; helm configuration
  (setq helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  ;; persistent undo ----------------------------------------
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,zv//undo-tree-directory)))

  (unless (file-exists-p zv//undo-tree-directory)
    (make-directory zv//undo-tree-directory))

  ;; abbrev-mode --------------------------------------------
  (setq-default abbrev-mode t))

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
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
 '(org-agenda-files
   (quote
    ("/home/zv/Documents/tasks.org" "/home/zv/Documents/agenda.org" "/home/zv/Documents/ledger.org" "/home/zv/Documents/norwars.org" "/home/zv/Documents/notes.org" "/home/zv/Documents/oldx.org" "/home/zv/Documents/quotes.org" "/home/zv/Documents/zv.org" "/home/zv/Documents/SFGOV/affordable_housing.org" "/home/zv/Documents/SFGOV/oracle_vouchers.org" "/home/zv/Documents/SFGOV/planning_commission.org")))
 '(org-html-text-markup-alist
   (quote
    ((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<del>%s</del>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<span class=\"verbatim\">%s</span>"))))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(ring-bell-function (quote ignore) t)
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
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.35 :family "Sans Serif")))))

