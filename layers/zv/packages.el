(defvar zv-packages '(;;jade-mode
                      ;;nodejs-repl
                      ))

(defvar zv-excluded-packages '())

(defun zv/init-bbdb ()
  (use-package bbdb
    :defer t
    :init (bbdb-initialize)
    :config (setq bbdb-expand-mail-aliases t
                  bbdb-complete-name-full-completion t
                  bbdb-file (concat user-emacs-directory "/bbdb.gpg"))))
