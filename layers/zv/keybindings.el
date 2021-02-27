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
(spacemacs/declare-prefix "oe" "extensions")
(spacemacs/set-leader-keys "oem" 'quick-calculate-mode)
(spacemacs/set-leader-keys "oeb" 'regexp-builder)

(spacemacs/set-leader-keys "oj" 'dumb-jump-go)
(spacemacs/set-leader-keys "ob" 'spacemacs-layouts/non-restricted-buffer-list-helm)

(spacemacs/declare-prefix "od" "documentation")
(spacemacs/set-leader-keys "odm" 'helm-man-woman)
(spacemacs/set-leader-keys "dm" 'man)

(spacemacs/set-leader-keys "jk" 'avy-goto-char)


;; Mode-specific leaders

;;; Python
(spacemacs/set-leader-keys-for-major-mode 'python-mode "ov" 'zv/search-parents-for-venv)


;; evil bindings
(global-set-key (kbd "H-f") 'evil-window-right)
(global-set-key (kbd "H-s") 'evil-window-left)
(global-set-key (kbd "H-e") 'evil-window-up)
(global-set-key (kbd "H-d") 'evil-window-down)

(global-set-key (kbd "H-k") 'evil-window-next)
(global-set-key (kbd "H-j") 'evil-window-prev)

(define-key evil-insert-state-map (kbd "C-y") 'yank)

;; (define-key evil-normal-state-map "\C-p" 'helm-projectile-find-file)
(define-key evil-normal-state-map (kbd "RET") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)

;; insert mode
(define-key evil-insert-state-map (kbd "C-h") 'backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'forward-char)

(global-set-key (kbd "H-w") 'zv/enlarge-window-by-dominant-dimension)
(global-set-key (kbd "H-r") 'zv/shrink-window-by-dominant-dimension)

;;(evil-define-key 'normal evil-surround-mode-map "s" 'evil-surround-region)

;; swap "{" with "[" & "}" with "]"
(dolist (mode (list evil-normal-state-map evil-motion-state-map evil-visual-state-map))
  (pcase-dolist (`(,ka . ,kb) '(("[" . "{") ("]" . "}")))
    (let ((funa (lookup-key mode ka))
          (funb (lookup-key mode kb)))
      (define-key mode ka funb)
      (define-key mode kb funa))))


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


; Man & WoMan
(use-package woman
  :bind (:map woman-mode-map
              ("}" . WoMan-next-manpage)
              ("{" . WoMan-previous-manpage)))

(use-package man
  :init
  (setq evil-lookup-func #'(lambda () (man (Man-default-man-entry))))
  :bind (:map Man-mode-map
              ("}" . Man-next-manpage)
              ("{" . Man-previous-manpage)
              ("u" . scroll-down-command)
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
              ("<M-down>"      . helm-ag--next-file)
              ("<M-up>"        . helm-ag--previous-file)
              ("<XF86Forward>" . helm-ag--next-file)
              ("<XF86Back>"    . helm-ag--previous-file)))


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
