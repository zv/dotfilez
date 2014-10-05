;;; ag -- summary
;;; Commentary:
;;; Code:
(prelude-require-package 'ag)
(require 'ag)
(setq ag-highlight-search t)
;

;;;
; Search
;;;
(setq ag-reuse-buffers 't)

(define-key evil-normal-state-map ",gr" 'helm-do-grep) ; grep
(define-key evil-normal-state-map ",ga" 'ag) ; grep
