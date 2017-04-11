;; -*- mode: emacs-lisp -*-
;;; keybindings.el --- zv contrib key-bindings File
;;
;; Copyright (c) 2012-2014 Zephyr Pellerin
;;
;; Author: zv <zv@nxvr.org>
;; URL: https://github.com/zv/spacemacs
;;
;;; License: GPLv3

;; global bindings
;; Forward/Backward mice keys
(global-set-key (kbd "<mouse-8>") 'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>") 'switch-to-next-buffer)

;; tab/window split manipulation]
(define-key evil-normal-state-map "Q" 'evil-quit)


;; Check for 'next-buffer-key && 'prev-buffer-key

                                        ; Hyper Key
(global-set-key (kbd "H-f") 'evil-window-right)
(global-set-key (kbd "H-s") 'evil-window-left)
(global-set-key (kbd "H-e") 'evil-window-up)
(global-set-key (kbd "H-d") 'evil-window-down)

(global-set-key (kbd "H-j") 'evil-window-next)
(global-set-key (kbd "H-k") 'evil-window-prev)

;; (global-set-key (kbd "C-H-j") (lambda () (interactive) (rotate-windows 1)))
;; (global-set-key (kbd "C-H-k") (lambda () (interactive) (rotate-windows -1)))
(global-set-key (kbd "H-w") (lambda ()
                              (interactive)
                              (zv/enlarge-window-by-dominant-dimension -7)))
(global-set-key (kbd "H-r") (lambda ()
                              (interactive)
                              (zv/enlarge-window-by-dominant-dimension 7)))
(global-set-key (kbd "C-H-<return>") 'zv/tile-split-window)

(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

;; EWW

(evil-define-key 'motion eww-mode-map
  "d" 'scroll-up-command
  "u" 'scroll-down-command
  "D" 'eww-download)

;; utilities
(global-set-key (kbd "C-x C-r") 're-builder)
(global-set-key (kbd "M-/") 'hippie-expand)

;; applications
(global-set-key (kbd "<XF86Calculator>") 'calc)
(global-set-key (kbd "<XF86Mail>") 'gnus)



;; evil state bindings
(evil-define-key 'normal evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'normal evil-surround-mode-map "S" 'evil-substitute)

;; (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

;; Although this is `helm' package configuration, this is more
;; accurately a keybinding for a very, very commonly used function
(use-package helm
  :defer t
  :init
  ;; I literally have never used the default `jj' keybinding.
  ;; Good riddance
  (evil-leader/set-key "jj" 'helm-mini))

;; insert mode
(define-key evil-insert-state-map (kbd "C-h") 'backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'forward-char)
;; (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
;; (define-key evil-insert-state-map (kbd "C-i") 'backward-delete-char)
;; (define-key evil-insert-state-map (kbd "C-s") 'undo-tree-undo)


;; evil-leader key bindings
;; Set our special "o" keys
(evil-leader/set-key
  "ob" 'spacemacs-layouts/non-restricted-buffer-list-helm)


;; mode bindings
;; js2 mode
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "H-s") 'add-semicolon-to-end-of-line)
     (define-key js2-mode-map next-buffer-key 'evil-window-next)
     (define-key js2-mode-map prev-buffer-key 'evil-window-prev)))

;; delete line
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-u") 'helm-delete-minibuffer-contents))

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map "\C-d" nil)))

;; Info Mode
(evil-add-hjkl-bindings Info-mode-map 'emacs
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  (kbd "M-h") 'Info-help   ; "h"
  ;;"j" 'zv/scroll-up-one-line
  ;;"k" 'zv/scroll-down-one-line
  "n" 'Info-search-next
  "/" 'Info-search
  "?" 'Info-search-backward
  "U" 'Info-up
  "D" 'Info-directory
  "u" 'Info-scroll-down
  "d" 'Info-scroll-up
  "p" 'evil-window-next ; "p"op out of info window
  "V" 'evil-visual-line
  "\C-u" 'Info-scroll-down
  "\C-d" 'Info-scroll-up
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  "\C-]" 'Info-follow-nearest-node
  ;; The following are for scroll up / scroll down keys
  (kbd "<mouse-4>") 'Info-scroll-down
  (kbd "<mouse-5>") 'Info-scroll-up
  (kbd "DEL") 'Info-scroll-down)


                                        ; dired
(with-eval-after-load 'dired
  ;; (evil-define-key 'normal dired-mode-map "s" 'dired-sort-toggle-or-edit)
  ;; (evil-define-key 'normal dired-mode-map "u" 'dired-up-directory)
  ;; (evil-define-key 'normal dired-mode-map "r" 'dired-unmark )
  ;; (evil-define-key 'normal dired-mode-map (kbd "<f5>") 'dired-do-redisplay)

  (setq-local
   ;; This ensures that `dired-other-window' doesn't split when using it as a
   ;; directory-manager.
   split-width-threshold 220)

  (evilified-state-evilify dired-mode dired-mode-map
    [mouse-1] 'diredp-find-file-reuse-dir-buffer
    [mouse-2] 'dired-find-alternate-file
    "f"  'helm-find-files
    "h"  'diredp-up-directory-reuse-dir-buffer
    "l"  'diredp-find-file-reuse-dir-buffer
    "c"  'helm-find-files

    "gg" 'evil-goto-first-line
    "G"  'evil-goto-line

    "j" 'dired-next-line
    "k" 'dired-previous-line
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "u" 'dired-up-directory
    "f" 'dired-goto-file
    "/" 'evil-search-forward
    "?" 'evil-search-backward
    "\C-u" 'evil-scroll-up
    "\C-d" 'evil-scroll-down
    "r" 'dired-unmark
    (kbd "<f5>") 'dired-do-redisplay))


                                        ; calendar
(with-eval-after-load 'calendar
  (setq diary-file (concat org-directory "events"))
  (add-hook 'calendar-mode-hook
            (lambda ()
              (define-key calendar-mode-map "l" 'calendar-forward-day)
              (define-key calendar-mode-map "h" 'calendar-backward-day)
              (define-key calendar-mode-map "j" 'calendar-forward-week)
              (define-key calendar-mode-map "k" 'calendar-backward-week)
              (define-key calendar-mode-map "{" 'calendar-forward-month)
              (define-key calendar-mode-map "}" 'calendar-backward-month)
              (define-key calendar-mode-map "0" 'calendar-beginning-of-week)
              (define-key calendar-mode-map "$" 'calendar-end-of-week)
              (define-key calendar-mode-map "[" 'calendar-beginning-of-month)
              (define-key calendar-mode-map "]" 'calendar-end-of-month)
              (define-key calendar-mode-map "gg" 'calendar-beginning-of-year)
              (define-key calendar-mode-map "G" 'calendar-end-of-year))))


; emacs vc
(with-eval-after-load 'vc
  (setq
   ;; Don't make backups of git history files
   vc-make-backup-files nil
   ;; Always follow a symlink inside of a git repository that slnz things
   vc-follow-symlinks t))


;; neotree
(with-eval-after-load "neotree"
  (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
  (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)
  )

;; speedbar
(spacemacs/set-leader-keys
  "oft" 'speedbar
  "am" 'man
  "oaw" 'woman)

(evil-define-key 'motion speedbar-file-key-map
  "l" 'speedbar-expand-line
  "h" 'speedbar-contract-line)

                                        ; Quitting Emacs Mode
(eval-after-load "view"
  (lambda ()
    (define-key view-mode-map (kbd "H-q") 'View-quit)))


                                        ; Manpages
(with-eval-after-load "Man"
  ;; Define our lookup funtion with `K' to be 'man' rather than 'woman'
  (setq evil-lookup-func #'man)
  (evil-define-motion evil-lookup ()
    (call-interactively evil-lookup-func))
  ;; Format our man pages with a width of 80 chars
  (setenv "MANWIDTH" "80")
  (define-key Man-mode-map " "    'scroll-up-command)
  (define-key Man-mode-map "\177" 'scroll-down-command)
  (define-key Man-mode-map "}"    'Man-next-section)
  (define-key Man-mode-map "{"    'Man-previous-section)
  (define-key Man-mode-map "]" 'evil-forward-paragraph)
  (define-key Man-mode-map "[" 'evil-backward-paragraph)
  (define-key Man-mode-map ">"    'end-of-buffer)
  (define-key Man-mode-map "<"    'beginning-of-buffer)
  (define-key Man-mode-map "."    'beginning-of-buffer)
  (define-key Man-mode-map "?"    'evil-search-backward)
  (define-key Man-mode-map "/"    'evil-search-forward)
  (define-key Man-mode-map "RET"  'woman-follow)

  (define-key Man-mode-map "n"    'evil-search-next)
  (define-key Man-mode-map "p"    'evil-search-previous)
  (define-key Man-mode-map "M-n"  'Man-next-section)
  (define-key Man-mode-map "M-p"  'Man-previous-section)

  (define-key Man-mode-map "k"    'evil-previous-line)
  (define-key Man-mode-map "j"    'evil-next-line)

  (define-key Man-mode-map "d"    'scroll-up-command)
  (define-key Man-mode-map "u"    'scroll-down-command)
  (define-key Man-mode-map "q"    'Man-quit)
  (define-key Man-mode-map "m"    'man))


;; Elixir Configuration
(defun zv/elixir-convert-def-to-block (&optional arg)
  (interactive "P")
  (re-search-forward "\\()?\\)\\s-*do\n\\s-*\\(.*\\)\n.*end" nil)
  (replace-match "\\1, do: \\2"))

(spacemacs|use-package-add-hook alchemist
  :post-config
  (progn
    (spacemacs/declare-prefix-for-mode 'elixir-mode "mk" "quickfixes")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode "kd"
      'zv/elixir-convert-def-to-block)))


;; Search
(spacemacs|use-package-add-hook helm-ag
  :post-config
  (progn
    (define-key helm-ag-map (kbd "<M-down>") 'helm-ag--next-file)
    (define-key helm-ag-map (kbd "<M-up>") 'helm-ag--previous-file)
    (define-key helm-ag-map (kbd "<XF86Forward>") 'helm-ag--next-file)
    (define-key helm-ag-map (kbd "<XF86Back>") 'helm-ag--previous-file)))

;; brackets
(define-key evil-normal-state-map "]" 'evil-forward-paragraph)
(define-key evil-normal-state-map "[" 'evil-backward-paragraph)
(define-key evil-motion-state-map "]" 'evil-forward-paragraph)
(define-key evil-motion-state-map "[" 'evil-backward-paragraph)
(define-key evil-visual-state-map "]" 'evil-forward-paragraph)
(define-key evil-visual-state-map "[" 'evil-backward-paragraph)

(define-key evil-normal-state-map "}" 'evil-forward-section-begin)
(define-key evil-normal-state-map "{" 'evil-backward-section-end)
(define-key evil-motion-state-map "}" 'evil-forward-section-begin)
(define-key evil-motion-state-map "{" 'evil-backward-section-end)
(define-key evil-visual-state-map "}" 'evil-forward-section-begin)
(define-key evil-visual-state-map "{" 'evil-backward-section-end)



                                        ; Initial State
(mapc (lambda (x) (evil-set-initial-state x 'emacs))
      '(epa-key-list-mode
        epa-key-mode
        epa-mail-mode
        Info-mode
        Man-mode
        term-mode
        calendar-mode
        racket-describe-mode))
