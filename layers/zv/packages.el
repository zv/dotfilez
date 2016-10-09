(defvar zv-packages '(magit
                      org
                      edts
                      erlang
                      eshell
                      z3-mode))

(defvar zv-excluded-packages '())


(defun zv/post-config-erlang ()
  (use-package erlang
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mh" "help/show")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "ms" "repl")
      (spacemacs/declare-prefix-for-mode 'scheme-mode "mg" "goto")

      (spacemacs/set-leader-keys-for-major-mode 'erlang-mode
        "hs" 'erlang-show-syntactic-information
        "si" 'erlang-shell-display
        "sc" 'erlang-compile-display
        "ga" 'erlang-beginning-of-clause
        "gf" 'erlang-beginning-of-function
        "y" 'erlang-clone-arguments
        "c" 'erlang-compile))))

(defun zv/init-edts ()
  (use-package edts
    :defer t
    :config
    (progn
      (setq-default
       edts-man-root "/usr/local/lib/erlang")
      )))

(defun zv/init-bbdb ()
  (use-package bbdb
    :defer t
    :init (bbdb-initialize)
    :config (setq bbdb-expand-mail-aliases t
                  bbdb-complete-name-full-completion t
                  bbdb-file (concat user-emacs-directory "/bbdb.gpg"))))

(defun zv/post-init-magit ()
  (with-eval-after-load 'magit
    (progn
      (evil-leader/set-key-for-mode 'magit-status-mode
        "mf" 'magit-key-mode-popup-gitflow)
      (define-key magit-mode-map "@" 'magit-branch-pull-request))))


(defun zv/post-init-eshell ()
  (with-eval-after-load 'eshell
    (progn
      (defalias 'e 'find-file)
      (defalias 'ff 'find-file)
      (defalias 'e 'find-file-other-window)
      (defalias 'gd 'magit-diff-unstaged)
      (defalias 'gds 'magit-diff-staged))))


(defun zv/post-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      ;; Use our custom org link insertion code
      ;; (define-key org-mode-map "\C-c\C-l" 'zv/org-insert-link)
      (require 'org-protocol)

      (setq-default
       ;; Do not dim blocked tasks
       org-agenda-dim-blocked-tasks nil
       ;; Compact the block agenda view
       org-agenda-compact-blocks t
       ;;open agenda in current window
       org-agenda-window-setup 'current-window
       ;;warn me of any deadlines in next 7 days
       org-deadline-warning-days 7
       ;;show me tasks scheduled or due in next fortnight
       org-agenda-span 'fortnight
       ;;don't show tasks as scheduled if they are already shown as a deadline
       org-agenda-skip-scheduled-if-deadline-is-shown t
       ;;don't give awarning colour to tasks with impending deadlines
       ;;if they are scheduled to be done
       org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
       ;;don't show tasks that are scheduled or have deadlines in the
       ;;normal todo list
       org-agenda-todo-ignore-deadlines 'all
       org-agenda-todo-ignore-scheduled 'all
       ;;sort tasks in order of when they are due and then by priority
       org-agenda-sorting-strategy '(((agenda deadline-up priority-down)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep)))

       org-log-done 'time
       org-todo-keywords '((sequence "TODO(t)" "STARTED" "|" "DONE(d!)" "CANCELLED(c@)")
                           (sequence "READ(r)" "READING" "|" "FINISHED(f!)"))

       ;; My shell variant is zsh
       org-babel-sh-command "zsh"

       ;; Priorities
       org-highest-priority ?A
       org-lowest-priority ?F
       org-default-priority ?C

       ;; include the emacs diary
       org-agenda-include-diary nil
       ;; Catch edits to invisible sections of the screen
       org-catch-invisible-edits t

       ;; Files
       org-capture-templates
       `(("a" "Appointment" entry (file+headline ,(concat org-directory "zv.org") "Appointments") "* APPT %^{Description} %^g\n %?\n Added: %U")
         ("b" "Book/Article" entry (file+headline ,(concat org-directory "zv.org") "Read") "** READ  %?")
         ;; ("i" "Ideas" entry (file+headline ,(concat org-directory "zv.org") "Ideas") "* %?\nCaptured On: %U\n")
         ("r" "Refile" entry (file+headline ,org-default-notes-file "Refile") "* %?\nCaptured On: %U\n")
         ("q" "Quotes" plain (file ,(concat org-directory "quotes.org")) "#+BEGIN_QUOTE\n%?\n#+END_QUOTE" :empty-lines 1)
         ;; Intentions don't have an active timestamp associated with them, but are marked as TODO items.
         ("i" "Intentions" entry (file+headline ,(concat org-directory "zv.org") "Tasks") "* TODO %?\nCaptured On: %U\n")
         ;; Tasks are things that I *need* to get done. They have a clock associated with them and an active timestamp. They appear in the Agenda.
         ("t" "Tasks" entry (file+headline ,(concat org-directory "zv.org") "Tasks") "* TODO %?\nCaptured On: %T\n"
          :clock-in t :clock-resume t)
         ;; Org Protocol
         ("L" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox") "* %? [[%:link][%:description]] \nCaptured On: %U")
         ("P" "Protocol" entry (file+headline ,org-default-notes-file "Inbox") "* %? [[%:link][%:description]] \n#+BEGIN_QUOTE\n%i\n#+END_QUOTE \nCaptured On: %U")
         ("T" "Thunderbird" entry (file+headline ,org-default-notes-file "Inbox") "* %? %:link\n#+BEGIN_QUOTE\n%:description\n#+END_QUOTE \nCaptured On: %U"))

       ;; ORG MODE PUBLISHING
       org-publish-project-alist `(("org-zv"
                                    :base-directory ,(concat zv//blog-path "org/")
                                    :base-extension "org"
                                    :publishing-directory ,zv//blog-path
                                    :publishing-function org-html-publish-to-html
                                    :html-container "section"
                                    :recursive t
                                    :section-numbers nil
                                    :with-toc nil
                                    :html-extension "html"
                                    :headline-levels 4
                                    :html-html5-fancy t
                                    :body-only t)
                                   ("zv-ghpages" :components ("org-zv")))

       ;; Targets include this file and any file contributing to the agenda - up to 6 levels deep
       org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
       org-refile-allow-creating-parent-node t
       org-refile-use-outline-path t
       ) ;; end of `set-default'

      ;; When running babel blocks in sh, I usually mean `zsh'
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((emacs-lisp . t)
                                     (dot . t)
                                     (ditaa . t)
                                     (python . t)
                                     (ruby . t)
                                     (clojure . t)
                                     (js . t)
                                     ))
      )))
