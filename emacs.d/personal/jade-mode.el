;;; jade-mode --- summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/personal/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;
