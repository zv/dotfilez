;; -*- mode: emacs-lisp -*-
;;; keybindings.el --- zv contrib key-bindings File
;;
;; Copyright (c) 2012-2014 Zephyr Pellerin
;;
;; Author: zv <zephyr.pellerin@gmail.com>
;; URL: https://github.com/zv/spacemacs

;; Forward/Backward mice keys
(global-set-key (kbd "<mouse-8>") 'switch-to-prev-buffer)
(global-set-key (kbd "<mouse-9>") 'switch-to-next-buffer)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(global-set-key (kbd "<XF86Calculator>") 'calc)
(global-set-key (kbd "<XF86Mail>") 'gnus)

;; tab/window split manipulation]
(define-key evil-normal-state-map "Q" 'evil-quit)

(define-key (current-global-map) [up] 'scroll-down-command)
(define-key (current-global-map) [down] 'scroll-up-command)


;; Custom leaders
(spacemacs/declare-prefix "o" "custom")
(spacemacs/declare-prefix "of" "custom-files")
(spacemacs/declare-prefix "ofe" "config" "Configuration files")
(zv/bind-find-file        "ofez" "~/dotfilez/"
                          "ofed" "~/Development/"
                          "ofez" zv-configuration-layer-directory)

(spacemacs/declare-prefix "ofo" "org-mode" "Documents and Org-mode files")
(zv/bind-find-file        "ofod" org-directory
                          "ofon" org-default-notes-file)

(spacemacs/declare-prefix "ofz" "blog" "Blogfolder")
(zv/bind-find-file        "ofzb" zv//blog-path
                          "ofzp" zv//blog-posts-path)

(spacemacs/declare-prefix "oo" "org-mode")
(spacemacs/set-leader-keys "oopa" #'(lambda ()
                                    (interactive)
                                    (print "Automatic publishing enabled")
                                    (add-hook 'after-save-hook 'zv/auto-publish)))
(spacemacs/set-leader-keys "oopr" #'(lambda ()
                                    (interactive)
                                    (print "Automatic publishing disabled")
                                    (remove-hook 'after-save-hook 'zv/auto-publish)))

(spacemacs/set-leader-keys "oj" 'dumb-jump-go)
(spacemacs/set-leader-keys "ob" 'spacemacs-layouts/non-restricted-buffer-list-helm)
(spacemacs/set-leader-keys "orb" 'regexp-builder)


;; evil bindings
(global-set-key (kbd "H-f") 'evil-window-right)
(global-set-key (kbd "H-s") 'evil-window-left)
(global-set-key (kbd "H-e") 'evil-window-up)
(global-set-key (kbd "H-d") 'evil-window-down)

(global-set-key (kbd "H-k") 'evil-window-next)
(global-set-key (kbd "H-j") 'evil-window-prev)

(define-key evil-insert-state-map (kbd "C-y") 'yank)

(evil-define-key 'normal evil-surround-mode-map "s" 'evil-surround-region)

;; (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

;; insert mode
(define-key evil-insert-state-map (kbd "C-h") 'backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'forward-char)

(global-set-key (kbd "H-w") 'zv/enlarge-window-by-dominant-dimension)
(global-set-key (kbd "H-r") 'zv/shrink-window-by-dominant-dimension)


;; swap "{" with "[" & "}" with "]"
(dolist (mode (list evil-normal-state-map evil-motion-state-map evil-visual-state-map))
  (dolist (pair '(("[" . "{") ("]" . "}")))
    (let* ((ka (car pair))
           (kb (cdr pair))
           (fa (lookup-key mode ka))
           (fb (lookup-key mode kb)))
      (define-key mode ka fb)
      (define-key mode kb fa))))


(use-package calendar
  :defer t
  :bind (:map calendar-mode-map
         ("l" . calendar-forward-day)
         ("h" . calendar-backward-day)
         ("j" . calendar-forward-week)
         ("k" . calendar-backward-week)
         ("{" . calendar-forward-month)
         ("}" . calendar-backward-month)
         ("0" . calendar-beginning-of-week)
         ("$" . calendar-end-of-week)))

(spacemacs|use-package-add-hook neotree
  :post-config
  (progn
    (define-key neotree-mode-map "I" 'neotree-hidden-file-toggle)
    (define-key evil-normal-state-map (kbd "C-\\") 'neotree-find)))

(evil-define-key 'motion speedbar-file-key-map
  "l" 'speedbar-expand-line
  "h" 'speedbar-contract-line)

(use-package man
  :defer t
  :bind (:map Man-mode-map
              (" " . scroll-up-command)
              ("\177" . scroll-down-command)
              ("}" . Man-next-section)
              ("{" . Man-previous-section)
              ("]" . evil-forward-paragraph)
              ("[" . evil-backward-paragraph)
              (">" . end-of-buffer)
              ("<" . beginning-of-buffer)
              ("." . beginning-of-buffer)
              ("?" . evil-search-backward)
              ("/" . evil-search-forward)
              ("RET" . woman-follow)

              ("n" . evil-search-next)
              ("p" . evil-search-previous)
              ("M-n" . Man-next-section)
              ("M-p" . Man-previous-section)

              ("k" . evil-previous-line)
              ("j" . evil-next-line)

              ("d" . scroll-up-command)
              ("u" . scroll-down-command)
              ("q" . Man-kill)
              ("m" . man)))


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


;; Set emacs as the initial state in a variety of modes
(dolist (mode '(epa-key-list-mode
                epa-key-mode
                epa-mail-mode
                Info-mode
                Man-mode
                term-mode
                calendar-mode
                racket-describe-mode))
  (evil-set-initial-state mode 'emacs))

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


;; Python bindings
(spacemacs|use-package-add-hook python
  :post-config
  (progn
    ;; searches the current buffer's directory and parents up to "/" for a directory
    ;; named "venv", activating it if it exists.
    (defun search-parents-for-venv ()
      (interactive)
      (defun venv-search (test-dir)
        (let ((venv-dir (expand-file-name (concat test-dir "/venv/"))))
          (cond
           ((string-equal test-dir "/") f)
           ((file-directory-p venv-dir) (pyvenv-activate venv-dir))
           (t (venv-search (file-name-directory (directory-file-name test-dir)))))))
      (venv-search (directory-file-name buffer-file-name)))

    ;; Set our custom keybindings
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "mv" 'search-parents-for-venv)))


(evil-define-key '(normal insert) 'quick-calculate-mode-map
  (kbd "M-r") 'calc-radix
  (kbd "M-k") 'zv/calculate-line
  (kbd "C-;") 'helm-calcul-expression)

(evil-define-key 'visual quick-calculate-mode-map
  (kbd "M-k") 'zv/calculate-region)
