;;---------------------------------------------------------------------
;; Configuration Layer Parameters
;;---------------------------------------------------------------------
(defconst zv-configuration-layer-directory (car dotspacemacs-configuration-layer-path)
  "zv contribution layer base directory.")

(setq spacemacs-repository "emacs.d")
(setq spacemacs-repository-owner "zv")

(setq-default
 ;; ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 ;; C Mode
 c-electric-mode t
 c-basic-offset  4
 ;; Javascript
 js2-global-externs '("module" "assert" "buster" "clearInterval" "clearTimeout" "console"
                      "__dirname" "JSON" "location" "refute" "require" "setInterval" "setTimeout"
                      "sinon" "Quad" "quad" "DS")
 js2-basic-offset                 2
 js2-strict-missing-semi-warning  nil
 js2-include-node-externs         t
 js2-include-browser-externs      t
 user-mail-address                "zv@nxvr.org"
 )

(defvar zv-whitelist '() "List of helm buffers in which to show dots.")


;; Additional emacs modes ------------------------------------
(mapc (lambda (x) (evil-set-initial-state x 'emacs))
      '(epa-key-list-mode
        epa-key-mode
        epa-mail-mode
        Info-mode
        Man-mode))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Fix this dagnabbit evil-escape f/t issue
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "t" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)

;; default browser used
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; tern
(setq tern-command '("node" "/bin/tern"))

;; GDB configuration
(setq gdb-many-windows t
      gdb-show-main t)

;; VC Mode
(eval-after-load 'vc
  '(progn
     (define-key vc-git-log-view-mode-map "j" 'log-view-msg-next)
     (define-key vc-git-log-view-mode-map "J" 'log-view-file-next)
     (define-key vc-git-log-view-mode-map "k" 'log-view-msg-prev)
     (define-key vc-git-log-view-mode-map "K" 'log-view-file-prev)
     (define-key vc-git-log-view-mode-map (kbd "<RET>") 'log-view-find-revision)))

(setq
 ;; Don't make backups of git history files
 vc-make-backup-files nil
 ;; Always follow a symlink inside of a git repository that slnz things
 vc-follow-symlinks t)

;; encrypt hook ------------------------------------------------------------------

"Install a hook to encrypt some files after saving"
(defun zv/encrypt-secrets ()
  "Encrypt this file if it is in one of our `dirs-to-encrypt'"
  (require 'epa-mail)
  (let* ((zv-dotfiles (expand-file-name "~/Development/dotfilez/"))
         (files-to-encrypt `(,(expand-file-name "~/.authinfo")))
         (dirs-to-encrypt `(,(expand-file-name "~/.gnupg")
                            ,(expand-file-name (concat org-directory "/"))
                            ,(concat zv-dotfiles "gnupg/")
                            ,(concat zv-dotfiles "ssh/")
                            ,(expand-file-name "~/.ssh/")))
         (recipient (epg-list-keys (epg-make-context epa-protocol) "<zv@nxvr.org>" 'public)))
    (when (or (member (file-name-directory (buffer-file-name)) dirs-to-encrypt) (member buffer-file-name files-to-encrypt))
      (epa-encrypt-file (buffer-file-name) recipient))))

;; Turn on encrypt hook
;; (add-hook 'after-save-hook 'zv/encrypt-secrets)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq dotspacemacs-themes '(solarized-dark
                            solarized-dark
                            leuven))

;; dired -----------------------------------------------------
(eval-after-load 'dired
  '(progn (define-key dired-mode-map "u" 'dired-up-directory )
          (define-key dired-mode-map "j" 'dired-next-line )
          (define-key dired-mode-map "k" 'dired-prev-line )
          (define-key dired-mode-map "f" 'dired-goto-file )
          (define-key dired-mode-map "\C-h" 'dired-tree-up)
          (define-key dired-mode-map "\C-l" 'dired-tree-down)
          (define-key dired-mode-map "\C-j"'dired-next-subdir)
          (define-key dired-mode-map "\C-k"'dired-prev-subdir)
          ;; dired-do-hardlink hard link [h]
          ;; dired-do-load
          (define-key dired-mode-map "r" 'dired-unmark )
          (define-key dired-mode-map (kbd "<f5>") 'dired-do-redisplay )))

(eval-after-load 'calendar
  '(progn
     (evil-set-initial-state 'calendar-mode 'emacs)
     (setq diary-file (concat org-directory "events"))
     (add-hook 'calendar-mode-hook
               (lambda ()
                 (define-key calendar-mode-map "l" 'calendar-forward-day)
                 (define-key calendar-mode-map "h" 'calendar-backward-day)
                 (define-key calendar-mode-map "j" 'calendar-forward-week)
                 (define-key calendar-mode-map "k" 'calendar-backward-week)
                 (define-key calendar-mode-map "{" 'calendar-forward-month)
                 (define-key calendar-mode-map "}" 'calendar-backward-month)
                 (define-key calendar-mode-map "0" 'calendar-beginning-of-week)
                 (define-key calendar-mode-map "$" 'calendar-end-of-week)
                 (define-key calendar-mode-map "[" 'calendar-beginning-of-month)
                 (define-key calendar-mode-map "]" 'calendar-end-of-month)
                 (define-key calendar-mode-map "gg" 'calendar-beginning-of-year)
                 (define-key calendar-mode-map "G" 'calendar-end-of-year)))))

(eval-after-load 'calc
  '(progn
     (define-key calc-mode-map next-buffer-key 'next-buffer)))

(eval-after-load 'eshell
  '(progn
     (require 'em-smart)
     ;; Ensure we set the path correctly
     (setq   eshell-path-env (concat "/!usr/local/bin" ":" eshell-path-env))
     ;; Ensure eshell
     (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
     (evil-define-key 'normal eshell-mode-map (kbd "C-p") 'eshell-previous-prompt)
     (evil-define-key 'normal eshell-mode-map (kbd "C-n") 'eshell-next-prompt)
     (evil-define-key 'normal eshell-mode-map (kbd "i") 'evil-insert-state)

     (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
           eshell-review-quick-commands   nil
           eshell-smart-space-goes-to-end t
           eshell-where-to-jump           'begin
           eshell-buffer-maximum-lines     20000
           eshell-buffer-shorthand         t)))


(eval-after-load 'web-mode
  '(progn
     ;; web-mode ------------------------------------------------
     (setq web-mode-enable-css-colorization t
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2)

     ;; (add-hook 'web-mode-hook (lambda () (turn-off-smartparens-mode)))
     ))

(eval-after-load 'eww
  '(progn
     (define-key eww-mode-map "q" 'eww-quit)
     (define-key eww-mode-map "\C-h" 'eww-previous-url)
     (define-key eww-mode-map "\C-l" 'eww-next-url)))

(use-package man
  :init
  (evil-set-initial-state 'Man-mode 'motion)
  :config
  ;; Define our lookup funtion with `K' to be 'man' rather than 'woman'
  (setq evil-lookup-func #'man)
  (evil-define-motion evil-lookup ()
    (call-interactively evil-lookup-func))
  ;; Format our man pages with a width of 80 chars
  (setenv "MANWIDTH" "80")

  (evil-define-key 'motion Man-mode-map
    " "    'scroll-up-command
    "\177" 'scroll-down-command
    "}"    'Man-next-section
    "{"    'Man-previous-section
    "]" 'evil-forward-paragraph
    "[" 'evil-backward-paragraph
    ">"    'end-of-buffer
    "<"    'beginning-of-buffer
    "."    'beginning-of-buffer
    "RET" 'woman-follow
    "d"    'scroll-up-command
    "u"    'scroll-down-command
    "gs"   'Man-goto-section
    "ga"   'Man-goto-see-also-section
    "q"    'Man-quit
    "m"    'man))



(spacemacs|use-package-add-hook org
  :post-config
  (progn
    ;; Use our custom org link insertion code
    (org-defkey org-mode-map "\C-c\C-l" 'zv/org-insert-link)

    (setq org-default-notes-file (expand-file-name "~/Documents/notes.org"))

    (setq org-agenda-files `("~/Documents" "~/Documents/SFGOV"))

    ;; org-mode agenda options
    ;; Do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)
    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)
    ;;open agenda in current window
    (setq org-agenda-window-setup (quote current-window))
    ;;warn me of any deadlines in next 7 days
    (setq org-deadline-warning-days 7)
    ;;show me tasks scheduled or due in next fortnight
    (setq org-agenda-span (quote fortnight))
    ;;don't show tasks as scheduled if they are already shown as a deadline
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
    ;;don't give awarning colour to tasks with impending deadlines
    ;;if they are scheduled to be done
    (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
    ;;don't show tasks that are scheduled or have deadlines in the
    ;;normal todo list
    (setq org-agenda-todo-ignore-deadlines (quote all))
    (setq org-agenda-todo-ignore-scheduled (quote all))
    ;;sort tasks in order of when they are due and then by priority
    (setq org-agenda-sorting-strategy
          (quote
           ((agenda deadline-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep))))

    (setq org-todo-keywords '((sequence "TODO" "STARTED" "DONE")))

    ;; include the emacs diary
    (setq org-agenda-include-diary t)
    ;; Catch edits to invisible sections of the screen
    (setq org-catch-invisible-edits t)

    ;; When running babel blocks in sh, I usually mean `zsh'
    (setq org-babel-sh-command "zsh")

    ;; APPT_WARNTIME property
    ;; Files
    (setq org-capture-templates
          `(("t" "Tasks" entry (file+headline ,(concat org-directory "tasks.org") "Tasks")
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
             "* %?\n\n  capture date: %U\n" :empty-lines 1)))

    ;; Targets include this file and any file contributing to the agenda - up to 6 levels deep
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (dot . t)
       (ditaa . t)
       (R . t)
       (python . t)
       (ruby . t)
       (gnuplot . t)
       (clojure . t)
       (sh . t)
       (ledger . t)
       (org . t)
       (plantuml . t)
       (latex . t)))

    ;; Priorities
    (setq org-highest-priority ?A
          org-lowest-priority ?F
          org-default-priority ?C)

    ;; Org mode keybindings
    ))

;; ORG MODE PUBLISHING
(spacemacs|use-package-add-hook org
  :post-config
  (progn
    (setq org-publish-project-alist
          `(("org-zv"
             :base-directory ,(concat zv//blog-path "org/")
             :base-extension "org"
             :publishing-directory ,zv//blog-path
             :publishing-function org-html-publish-to-html
             :html-container "section"
             :recursive t
             :section-numbers nil
             :with-toc nil
             :html-extension "html"
             :html-html5-fancy t
             :body-only t)
            ("zv-ghpages" :components ("org-zv"))))))

;; -------------------------------------------------------------------------
;;; Emacs support for hacking on NetworkManager
;; -------------------------------------------------------------------------

(c-add-style "NetworkManager"
             '(
                                        ; Start with the "bsd" style
               "bsd"

                                        ; ...but remove the rule saying labels must be indented at
                                        ; least one space
               (c-label-minimum-indentation . 0)

                                        ; 4-space tabs/indents
               (tab-width . 4)
               (c-basic-offset . 4)

                                        ; Use smart-tabs-mode (see below) to get tabs for indentation
                                        ; but spaces for alignment of continuation lines.
               (smart-tabs-mode . t)

                                        ; Multi-line "if" conditions are indented like this:
                                        ;     if (   foo
                                        ;         && bar)
                                        ; (You have to add the spaces on the first line yourself, but
                                        ; this will make emacs align the "&&" correctly.)
               (c-offsets-alist (arglist-cont-nonempty . (nm-lineup-arglist))
                                (arglist-close . (nm-lineup-arglist)))

                                        ; NM's comments use two spaces after a period and are
                                        ; (generally) wrapped at 80 characters
               (sentence-end-double-space . t)
               (fill-column . 80)))

;; http://www.emacswiki.org/emacs/SmartTabs
;; (require 'smart-tabs-mode)

;; The smart-tabs-mode documentation tells you to use
;; smart-tabs-insinuate to set it up, but that will cause it to be
;; enabled for *all* C code. We only want to enable it for
;; NetworkManager, so we have to manually set it up first.
;;(smart-tabs-advice c-indent-line c-basic-offset)
;;(smart-tabs-advice c-indent-region c-basic-offset)

;; Implements the weird "if" alignment
(defun nm-lineup-arglist (langelem)
  (save-excursion
    (back-to-indentation)
    (c-go-up-list-backward)
    (vector (+ (current-column) 1))))


(dir-locals-set-class-variables 'nm '((c-mode . ((c-file-style . "NetworkManager")))))

;; Now add a line like the following for every directory where you want the
;; "NetworkManager" style to be the default

                                        ; (dir-locals-set-directory-class "/home/danw/gnome/NetworkManager/" 'nm)
                                        ; (dir-locals-set-directory-class "/home/danw/gnome/network-manager-applet/" 'nm)

;; -------------------------------------------------------------------------
;;;
;; -------------------------------------------------------------------------
(with-eval-after-load 'helm-files
  (advice-add 'helm-ff-filter-candidate-one-by-one
              :around 'zv/helm-ff-filter-candidate-one-by-one)
  (advice-add 'helm-find-files-up-one-level
              :around 'zv/helm-find-files-up-one-level))


;; smartparens
;; (eval-after-load 'smartparens
;;   '(progn
;;      (sp-pair "(" nil :actions :rem)
;;      (sp-pair "<" nil :actions :rem)
;;      (sp-pair "[" nil :actions :rem)
;;      (sp-pair "'" nil :actions :rem)
;;      (sp-pair "\"" nil :actions :rem)))

