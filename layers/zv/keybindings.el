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
(if (and (boundp 'next-buffer-key) (boundp 'prev-buffer-key))
    (progn
      (global-set-key next-buffer-key 'evil-window-next)
      (global-set-key prev-buffer-key 'evil-window-prev)
      (global-set-key (kbd "C-H-j") (lambda () (interactive) (rotate-windows 1)))
      (global-set-key (kbd "C-H-k") (lambda () (interactive) (rotate-windows -1)))
      (global-set-key (kbd "H-h") (lambda ()
                                    (interactive)
                                    (zv/enlarge-window-by-dominant-dimension -7)))
      (global-set-key (kbd "H-l") (lambda ()
                                    (interactive)
                                    (zv/enlarge-window-by-dominant-dimension 7)))
      (global-set-key (kbd "C-H-<return>") 'zv/tile-split-window)))

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

;; previously was evil-lookup
(define-key evil-normal-state-map "K" 'zv/join-up)


;; evil state bindings
(setq evil-cross-lines t)
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
  "am" 'man

  "oaw" 'woman
  "oag" 'gnus
  "oo" 'org-capture
  "oa" 'org-agenda
  "oc" 'org-clock-in)

(defun zv//initial-path-keybinding (key-file-map)
  "Create leader keybindings from an alist of the form (KEYS . PATH)"
  (mapc (lambda (binding)
          (let* ((path        (cdr binding))
                 (keybinding  (car binding)))
            ;; We check if it is an integer because keyseq returns a number if
            ;; the preceeding keys are also unbound.
            (evil-leader/set-key keybinding (if (string-match "\/$" path)
                                                ;; use ido-find-file-in-dir if we're binding a directory
                                                `(lambda () (interactive) (ido-find-file-in-dir ,path))
                                              ;; Otherwise we're looking at a file, jump directly to it
                                              `(lambda () (interactive) (find-file-existing ,path))))))
  key-file-map))

(zv//initial-path-keybinding `(("ofez" . ,zv-configuration-layer-directory)
                               ("ofel" . "~/Development/")
                               ("ofzd" . "~/dotfilez/")
                               ("ofzo" . ,org-directory)
                               ("ofzn" . ,(concat org-directory "/notes.org"))
                               ("ofzb" . ,zv//blog-path)
                               ("ofzp" . ,(concat zv//blog-path "org/_posts/"))))


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

;; Autocomplete
(eval-after-load 'auto-complete
  '(global-set-key (kbd "<backtab>") 'ac-start))

(eval-after-load 'company
  '(progn
     ;; Don't forget C-s (search candidates)
     (global-set-key (kbd "<backtab>") 'company-complete)))

(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map "\C-d" nil)))

;; Info Mode
(evil-add-hjkl-bindings Info-mode-map 'emacs
  "0" 'evil-digit-argument-or-evil-beginning-of-line
  (kbd "M-h") 'Info-help   ; "h"
  "/" 'Info-search
  "?" 'Info-search-backward
  "U" 'Info-up
  "D" 'Info-directory
  "u" 'Info-scroll-down
  "d" 'Info-scroll-up
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
  (evil-define-key 'normal dired-mode-map "u" 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "j" 'dired-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'dired-prev-line)
  (evil-define-key 'normal dired-mode-map "f" 'dired-goto-file)
  (evil-define-key 'normal dired-mode-map "\C-h" 'dired-tree-up)
  (evil-define-key 'normal dired-mode-map "\C-l" 'dired-tree-down)
  (evil-define-key 'normal dired-mode-map "\C-j"'dired-next-subdir)
  (evil-define-key 'normal dired-mode-map "\C-k"'dired-prev-subdir)
  ;; dired-do-hardlink hard link [h]
  ;; dired-do-load
  (evil-define-key 'normal dired-mode-map "r" 'dired-unmark )
  (evil-define-key 'normal dired-mode-map (kbd "<f5>") 'dired-do-redisplay))


                                        ; calendar
(with-eval-after-load 'calendar
  (progn
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
                (define-key calendar-mode-map "G" 'calendar-end-of-year)))))


; emacs vc
(with-eval-after-load 'vc
  (define-key vc-git-log-view-mode-map "j" 'log-view-msg-next)
  (define-key vc-git-log-view-mode-map "J" 'log-view-file-next)
  (define-key vc-git-log-view-mode-map "k" 'log-view-msg-prev)
  (define-key vc-git-log-view-mode-map "K" 'log-view-file-prev)
  (define-key vc-git-log-view-mode-map (kbd "<RET>") 'log-view-find-revision)

  (setq
   ;; Don't make backups of git history files
   vc-make-backup-files nil
   ;; Always follow a symlink inside of a git repository that slnz things
   vc-follow-symlinks t))


;; neotree
(with-eval-after-load "neotree"
  (lambda ()
    (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
    (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)))

;; speedbar
(spacemacs/set-leader-keys
  "oft" 'speedbar)

(evil-define-key 'motion speedbar-file-key-map
  "l" 'speedbar-expand-line
  "h" 'speedbar-contract-line)

(eval-after-load "view"
  (lambda ()
    (define-key view-mode-map (kbd "H-q") 'View-quit)))


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
    (define-key helm-ag-map (kbd "<XF86Forward>") 'helm-ag--next-file)
    (define-key helm-ag-map (kbd "<XF86Back>") 'helm-ag--previous-file)))

;; brackets
(define-key evil-normal-state-map "]" 'evil-forward-paragraph)
(define-key evil-normal-state-map "[" 'evil-backward-paragraph)
(define-key evil-motion-state-map "]" 'evil-forward-paragraph)
(define-key evil-motion-state-map "[" 'evil-backward-paragraph)


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
