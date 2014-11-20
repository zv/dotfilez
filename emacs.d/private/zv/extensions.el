(defvar zv-pre-extensions
  '(
    ;; pre extension zvs go here
    )
  "List of all extensions to load before the packages.")

(defvar zv-post-extensions
  '(
    ;; post extension zvs go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function zv/init-<extension-zv>
;;
;; (defun zv/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
