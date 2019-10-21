;; -*- mode: emacs-lisp -*-

;; Emacs global configuration
(defconst zv-configuration-layer-directory (car dotspacemacs-configuration-layer-path)
  "zv contribution layer base directory.")

(defvar zv-whitelist '() "List of helm buffers in which to show dots.")

(defconst zv//blog-path (expand-file-name "~/zv.github.com/")
  "Path to the root of the org blog")

(defvar zv//blog-posts-path (expand-file-name "org/_posts/" zv//blog-path)
  "Path to the location of blog posts")

(define-minor-mode quick-calculate-mode
  "Local minor mode for quickly evaluating mathematical expressions inline"
  :keymap '())

(defvar zv/manpage-map
  (let ((map (make-sparse-keymap)))
    (define-key map "]" 'Man-next-section)
    (define-key map "[" 'Man-previous-section)
    (define-key map "g" 'Man-goto-section)
    (define-key map "r" 'Man-follow-manual-reference)
    (define-key map "s" 'Man-goto-see-also-section)
    (define-key map "?" 'evil-ex-search-backward)
    (define-key map "/" 'evil-ex-search-forward)
    (define-key map "n" 'evil-ex-search-next)
    (define-key map "N" 'evil-ex-search-backward)
    (define-key map "w" 'evil-forward-word-begin)
    (define-key map "W" 'evil-forward-WORD-begin)
    (define-key map "b" 'evil-backward-word-begin)
    (define-key map "B" 'evil-backward-WORD-begin)
    (define-key map "h" 'evil-backward-char)
    (define-key map "l" 'evil-forward-char)
    (define-key map "k" 'evil-previous-line)
    (define-key map "j" 'evil-next-line)
    (define-key map "d" 'scroll-up-command)
    (define-key map "u" 'scroll-down-command)
    (define-key map "Q" 'Man-kill)
    (define-key map "q" 'quit-window)
    map)
  "Keymap with bindings common to all manpage-viewing modes")
