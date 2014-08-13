;;; prelude-key-chord.el --- Key chord setup
;;; Commentary:
;   Configure key-chord key bindings.
;;; Code:
;; Key Chord bindings
; (prelude-require-package 'key-chord)

;;;
; Evil
;;;
; (prelude-require-package 'evil-leader)
;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key evil-normal-state-map (kbd "C-c C-b") 'ibuffer)
;(global-evil-leader-mode)
; <leader><leader>w for easymotion
(setq evil-cross-lines t)
(define-key evil-normal-state-map (kbd "C-t") 'previous-buffer)
(define-key evil-visual-state-map ",a" 'align-regexp) ; to align by regex
(define-key evil-normal-state-map "s" 'ace-jump-char-mode)
(define-key evil-visual-state-map "s" 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'evil-scroll-up)
; (define-key evil-normal-state-map ",,w" 'ace-jump-word-mode)
; (define-key evil-visual-state-map ",,w" 'ace-jump-word-mode)
; <C-u> is used for a lot of other stuff
; (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
; done

; H/L should go to the first / last non blank character respectively
(define-key evil-visual-state-map "L" 'evil-last-non-blank)
(define-key evil-visual-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)

;;;
; Narrowing
;;; (or ,nr)
(define-key evil-visual-state-map ",nn" 'narrow-to-region)
(define-key evil-normal-state-map ",nw" 'widen)
(define-key evil-normal-state-map ",np" 'narrow-to-page)

;;;
; Etags
;;;
; Jump to a (list of) tags
(define-key evil-normal-state-map "g]" 'helm-etags-select)
(define-key evil-visual-state-map "g]" 'helm-etags-select)

;;;
; Projectile
;;;
; Display a list of known projects you can switch to.
(define-key evil-normal-state-map ",pr" 'projectile-switch-project)

;;;
; Frames
;;;
(define-key evil-normal-state-map ",fc" 'make-frame-command)  ; create frame
(define-key evil-normal-state-map ",fd" 'delete-frame)        ; destroy frame
(define-key evil-normal-state-map ",fo" 'delete-other-frames) ; only frame

(define-key evil-normal-state-map ",gs" 'magit-status) ; <leader> [g]it [status]

; ,u for undo tree
(define-key evil-normal-state-map ",u" 'undo-tree-visualize)

; ,k for man-list
(define-key evil-normal-state-map ",k" 'evil-lookup)

;;;
; Ag.el
;;;
(define-key ag-mode-map "k" 'evil-previous-line)
;;;;
;; Calendar
;;;;
(require 'calendar)
(evil-ex-define-cmd "cal" 'calendar)
(evil-set-initial-state 'calendar-mode 'emacs)
(define-key calendar-mode-map "l" 'calendar-forward-day)
(define-key calendar-mode-map "h" 'calendar-backward-day)
(define-key calendar-mode-map "j" 'calendar-forward-week)
(define-key calendar-mode-map "k" 'calendar-backward-week)

;;;
; Man Pages
;;;
(evil-set-initial-state 'Man-mode 'emacs)

;;;; ;;;; REMEMBER THIS
;; (evil-set-initial-state mode state))
;;; End EVIL

;;;
; Autocomplete
;;;
(define-key company-active-map "\C-n" 'company-select-next)
(define-key company-active-map "\C-p" 'company-select-previous)

;;;
; CTRLp
;;;
(define-key evil-normal-state-map "\C-p" 'projectile-find-file)

;;;
; Commenting
;;;
(define-key evil-visual-state-map "gc" 'comment-region)

;;;
; Window / Tab Split Manipulation
;;;
(global-set-key "\C-h" 'evil-window-left)
(global-set-key "\C-l" 'evil-window-right)
(global-set-key "\C-j" 'evil-window-down)
(global-set-key "\C-k" 'evil-window-up)

;;;
; Snippet Dirs
;;;
(setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/snippets")))


;
