;;; javascript.el - JavaScript setup
;;; Commentary:
;   Enable js3-mode and other javascript settings
;;; Code:
;; Neotree settings

(prelude-require-package 'js3-mode)
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(add-hook 'js3-mode-hook
        (lambda ()
          (setq js3-auto-indent-p t
                js3-curly-indent-offset 0
                js3-enter-indents-newline t
                js3-expr-indent-offset 2
                js3-indent-on-enter-key t
                js3-lazy-commas t
                js3-lazy-dots t
                js3-mirror-mode t
                js3-lazy-operators t
                js3-paren-indent-offset 2
                js3-include-browser-externs t
                js3-global-externs '("$" "console" "require" "module"
                                     "exports" "_" "jQuery", "process")
                js3-square-indent-offset 4)

          (define-key evil-normal-state-map "gt" 'tern-get-type)
          (define-key evil-normal-state-map "gd" 'tern-find-definition)
          (setq flycheck-idle-change-delay 1.0)
          (setq tab-always-indent t)
          (linum-mode 1)))

        ;; https://github.com/Fuco1/smartparens/issues/239
        ;; (defadvice js3-enter-key (after fix-sp-state activate)
        ;;   (setq sp-last-operation 'sp-self-insert))

        ;; (sp-local-pair 'js3-mode
        ;;                "{"
        ;;                nil
        ;;                :post-handlers
        ;;                '((zv-create-newline-and-enter-sexp js3-enter-key))))


; C-c C-r : Rename variable
; C-c C-c : Get type
; C-c C-d : Get Docs

(prelude-require-package 'tern)
(add-hook 'js3-mode-hook (lambda () (tern-mode t)))

(add-hook 'js3-mode-hook (lambda () (abbrev-mode 1)))

(prelude-require-package 'company-tern)

; tern-find-definition
;; M-. Jump to the definition of the thing under the cursor.
;; M-, Brings you back to last place you were when you pressed M-..
;; C-c C-r Rename the variable under the cursor.
;; C-c C-c Find the type of the thing under the cursor.
;; C-c C-d Find docs of the thing under the cursor. Press again to open the associated URL (if any).


; Add `company-tern' to allowed `company-mode' backends list

(add-to-list 'company-backends 'company-tern)

; If you don't like circles after object's own properties consider less
; annoying marker for that purpose.

; (setq company-tern-property-marker "")

; You can trim too long function signatures to the frame width.

; (setq company-tern-meta-as-single-line t)

; If you doesn't like inline argument annotations appear with
; corresponding identifiers, then you can to set up the company align
; option.

; (setq company-tooltip-align-annotations t)

;;;
; Coffee-mode
;;;

(prelude-require-package 'coffee-mode)
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\Cakefile$" . coffee-mode))

(eval-after-load 'coffee-mode
  '(progn
     ;; CoffeeScript uses two spaces.
     (setq coffee-tab-width 2)

     ;; If you don't have js2-mode
     (setq coffee-js-mode 'js3-mode)

     ;; If you don't want your compiled files to be wrapped
     (setq coffee-args-compile '("-c" "--bare"))

     ;; *Messages* spam
     (setq coffee-debug-mode t)

     ;; Emacs key binding
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

     (setq coffee-command "coffee")

     (defun prelude-coffee-mode-defaults ()
       "coffee-mode-defaults"
       ;; Compile '.coffee' files on every save
       (and (buffer-file-name)
            (file-exists-p (buffer-file-name))
            (file-exists-p (coffee-compiled-file-name (buffer-file-name)))
            (coffee-cos-mode t))
       (subword-mode +1))

     (setq prelude-coffee-mode-hook 'prelude-coffee-mode-defaults)
    )
)

;
