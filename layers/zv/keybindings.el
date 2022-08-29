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
(spacemacs/declare-prefix "ofe" "config")
(zv/bind-find-file        "ofez" "~/dotfilez/"
                          "ofed" "~/Development/")

(spacemacs/declare-prefix "ofo" "org-mode")
(zv/bind-find-file        "ofod" org-directory
                          "ofon" org-default-notes-file)

(spacemacs/declare-prefix "ofz" "blog")
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
(spacemacs/set-leader-keys "jk" 'avy-goto-char)


;; Use the "smiley button" to switch between windows
(global-set-key (kbd "C-M-s-SPC") 'evil-window-next)

;; (define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
;; (define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

(use-package calendar
  :defer t
  :bind (:map calendar-mode-map
         ("l" . 'calendar-forward-day)
         ("h" . 'calendar-backward-day)
         ("j" . 'calendar-forward-week)
         ("k" . 'calendar-backward-week)
         ("{" . 'calendar-forward-month)
         ("}" . 'calendar-backward-month)
         ("0" . 'calendar-beginning-of-week)
         ("$" . 'calendar-end-of-week)))


; Man & WoMan
(use-package woman
  :bind (:map woman-mode-map
              ("}" . 'WoMan-next-manpage)
              ("{" . 'WoMan-previous-manpage)))

(use-package man
  :init
  (setq evil-lookup-func #'(lambda () (man (Man-default-man-entry))))
  :bind (:map Man-mode-map
              ("}" . 'Man-next-manpage)
              ("{" . 'Man-previous-manpage)
              ("u" . 'scroll-down-command)
              ("]" . 'Man-next-section)
              ("[" . 'Man-previous-section)
              ("g" . 'Man-goto-section)
              ("r" . 'Man-follow-manual-reference)
              ("s" . 'Man-goto-see-also-section)
              ("?" . 'evil-ex-search-backward)
              ("/" . 'evil-ex-search-forward)
              ("n" . 'evil-ex-search-next)
              ("N" . 'evil-ex-search-backward)
              ("V" . 'evil-visual-line)
              ("v" . 'evil-visual-char)
              ("w" . 'evil-forward-word-begin)
              ("W" . 'evil-forward-WORD-begin)
              ("b" . 'evil-backward-word-begin)
              ("B" . 'evil-backward-WORD-begin)
              ("h" . 'evil-backward-char)
              ("l" . 'evil-forward-char)
              ("k" . 'evil-previous-line)
              ("j" . 'evil-next-line)
              ("d" . 'scroll-up-command)
              ("u" . 'scroll-down-command)
              ("Q" . 'Man-kill)
              ("q" . 'quit-window)))


;; Search
(use-package helm-ag
  :defer t
  :bind (:map helm-ag-map
              ("<M-down>"      . 'helm-ag--next-file)
              ("<M-up>"        . 'helm-ag--previous-file)
              ("<XF86Forward>" . 'helm-ag--next-file)
              ("<XF86Back>"    . 'helm-ag--previous-file)))


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

;; Ensure that view mode's keybindings are respected
(add-hook 'view-mode-hook 'evil-motion-state)

;; Info Mode
(evil-add-hjkl-bindings Info-mode-map 'emacs
  (kbd "M-h") 'Info-help   ; "h"
  "n" 'Info-search-next
  "/" 'Info-search
  "?" 'Info-search-backward
  "U" 'Info-up
  "u" 'Info-scroll-down
  "d" 'Info-scroll-up
  "\C-u" 'Info-scroll-down
  "\C-d" 'Info-scroll-up
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  "\C-]" 'Info-follow-nearest-node
  "\C-e" 'Info-edit-mode)
