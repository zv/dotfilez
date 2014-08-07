;;; ag -- summary
;;; Commentary:
;;; Code:
(prelude-require-package 'ag)
; A package for integrating ag with helm
(prelude-require-package 'helm-ag)
(require 'ag)
(setq ag-highlight-search t)

;;;
; Search
;;;
(setq ag-reuse-buffers 't)
(define-key evil-normal-state-map ",gr" 'helm-do-ag)
(define-key evil-normal-state-map ",ga" 'ag)

;
