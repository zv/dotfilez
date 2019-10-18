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
