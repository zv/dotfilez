;; -*- mode: emacs-lisp -*-
(setq-default
 ;; Directory to use for ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ;; IRC commands to ignore in tracking
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 ;; List of additional paths where to look for configuration layers.
 ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
 dotspacemacs-configuration-layer-path '()
 ;; List of contribution to load.
 dotspacemacs-configuration-layers '(zv erlang-elixir)
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
          ("q" "Quotes" item (file (concat org-directory "quotes.org")) "%?"))))

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
  (setenv "PATH"          (concat "/usr/local/bin" ":" (getenv "PATH")))
  (setq evilnc-hotkey-comment-operator "gc")
)

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

  ;; evil --------------------------------------------------------------------
  ;; evil-leader breaks this, now we fix it.
  ;; (define-key evil-motion-state-map "f" 'evil-find-char)
  (setq evil-cross-lines t)
  ;; As I never distinguish between [[ & [{, I might as well get the
  ;; benefit of use of the easier one
  (define-key evil-motion-state-map "]" 'evil-forward-section-begin)
  (define-key evil-motion-state-map "[" 'evil-backward-section-begin)
  (define-key evil-motion-state-map "(" 'evil-previous-open-paren)
  (define-key evil-motion-state-map ")" 'evil-next-close-paren)
  ;; tab/window split manipulation --------------------------------------------
  (global-set-key "\C-h" 'evil-window-left)
  (global-set-key "\C-l" 'evil-window-right)
  (global-set-key "\C-j" 'evil-window-down)
  (global-set-key "\C-k" 'evil-window-up)

  ;; relative line numbers ------------------------------------------------------
  (global-linum-mode 1)
  (linum-relative-toggle)

  ;; js2-configuration -------------------------------------------------------
  (require 'js2-mode)
  (define-key js2-mode-map (kbd "C-;") 'add-semicolon-to-end-of-line)
  (setq js2-global-externs '("module"
                             "assert"
                             "buster"
                             "clearInterval"
                             "clearTimeout"
                             "console"
                             "__dirname"
                             "JSON"
                             "location"
                             "refute"
                             "require"
                             "setInterval"
                             "setTimeout"
                             "sinon"))
  (setq js2-basic-offset                 2
        js2-include-node-externs         t
        js2-include-browser-externs      t)


  (evil-set-initial-state 'Man-mode 'emacs)

  ; Org-Mode keybindings
  (evil-leader/set-key
    "oc" 'org-capture
    "oa" 'org-agenda
    "osl" 'org-store-link)

  ; Align keybinding
  (evil-leader/set-key "al" 'align-regexp)

  ;; Info Mode ----------------------------------------------------------------
  (evil-add-hjkl-bindings Info-mode-map 'motion
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    (kbd "\M-h") 'Info-help   ; "h"
    "\C-t" 'Info-history-back ; "l"
    "\C-o" 'Info-history-back
    "\C-]" 'Info-follow-nearest-node
    (kbd "RET") 'Info-scroll-up 
    (kbd "DEL") 'Info-scroll-down)

  ;; Helm Configuration --------------------------------------------------------
  (setq helm-quick-update                     t
        helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-bookmark-show-location           t
        helm-move-to-line-cycle-in-source     nil
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t
        )
  ;; Configure Modeline Colors -------------------------------------------------
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t)))
  ;; Configure Modeline Colors -------------------------------------------------
 (mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
                '((normal . "DarkGoldenrod2")
                  (insert . "chartreuse3")
                  (emacs  . "SkyBlue2")
                  (visual . "gray")
                  (motion . "plum3")
                  (lisp   . "HotPink1"))) 

 ;; eldoc ----------------------------------------------------------------------
 (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
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
 '(ahs-inhibit-face-list nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(edts-man-root "/home/zv/.emacs.d/edts/doc/17.3")
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
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
