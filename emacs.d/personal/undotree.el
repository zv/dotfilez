; Undotree configuration
;;; Code:
(setq-default undo-tree-visualizer-diff t)

(define-key evil-normal-state-map ",u" 'undo-tree-visualize) ; ,u for undo tree
;
