(defvar zv-packages '(magit
                      org))

(defvar zv-excluded-packages '())

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


(defun zv/post-init-org ()
  (with-eval-after-load 'magit
    (progn
      ;; Use our custom org link insertion code
      (define-key org-mode-map "\C-c\C-l" 'zv/org-insert-link)

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

       org-todo-keywords '((sequence "TODO" "STARTED" "DONE"))

       ;; My shell variant is zsh
       org-babel-sh-command "zsh"

       ;; Priorities
       org-highest-priority ?A
       org-lowest-priority ?F
       org-default-priority ?C

       ;; include the emacs diary
       org-agenda-include-diary t
       ;; Catch edits to invisible sections of the screen
       org-catch-invisible-edits t

       ;; Files
       org-capture-templates `(("t" "Tasks" entry (file+headline ,(concat org-directory "tasks.org") "Tasks")
                                "* TODO [#A] %?\nSCHEDULED: %t\n")
                               ;; Quotes
                               ("k" "Quotes" plain
                                (file (concat org-directory "quotes.org"))
                                "#+BEGIN_QUOTE\n%?\n#+END_QUOTE" :empty-lines 1)
                               ;; Snippets (%x copies from clipboard)
                               ("s" "Code Snippet" entry (file+headline ,org-default-notes-file "Snippets")
                                "* %?\n#+BEGIN_SRC %^{prompt}\n%x\n#+END_SRC")
                               ;; Ideas
                               ("i" "Ideas" entry
                                (file+headline ,org-default-notes-file "Ideas")
                                "* %? %^G\n" :clock-in t :clock-resume t)
                               ;; awesome list
                               ("m" "Awesome List" entry
                                (file+headline ,org-default-notes-file "awesome-list")
                                "** %?\n\n capture date: %U\n")
                               ;; Note / Remember
                               ("n" "Note" entry
                                (file ,org-default-notes-file)
                                "* %?\n\n  capture date: %U\n" :empty-lines 1))

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
       org-refile-targets '((org-agenda-files . (:maxlevel . 6))))


      ;; When running babel blocks in sh, I usually mean `zsh'
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((emacs-lisp . t)
                                     (dot . t)
                                     (ditaa . t)
                                     (python . t)
                                     (ruby . t)
                                     (clojure . t)
                                     ;; (sh . t)
                                     ;; (plantuml . t)
                                     ;; (latex . t)
                                     (ledger . t)
                                     ;; (org . t)
                                     ))
      )))
