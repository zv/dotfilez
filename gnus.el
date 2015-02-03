 ;; jeremykun
;; nodeweekly
;; emberweekly
;; javascriptweekly
;; dailyjs
;; sitepoint
;; ajaxian
;; addy osmandi
;; statuscode
;; ericlippert 

;; ember weekly http://us4.campaign-archive2.com/feed?u=ac25c8565ec37f9299ac75ca0&id=e96229d21d

;; TODO:
;; add key to message mode to go back to group
;; regularly check gmail for new messges
;; Primary Mailbox (GMane)
;; :q shoudl exit gnus
(setq gnus-select-method '(nntp "news.gmane.org")) 

;;; Ask encyption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Use our auth-source file
(setq smtpmail-auth-credentials "~/.authinfo.gpg")

(load "/home/zv/.emacs.d/contrib/gnus/funcs.el")

;;@see http://gnus.org/manual/gnus_397.html
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-fetch-partial-articles t)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)))

;; All Gmail groups will be ignored by the default value of
;; gnus-ignored-newsgroups, so let’s change that default value.
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

;; fast searching and nnheader
;;
(require 'nnir)
(require 'nnheader)

;; Multiple SMTP servers
(setq smtp-accounts
      '(("zv@nxvr.org" "zephyr" "mail.nxvr.org" 10025)
        ("zephyr.pellerin@gmail.com" "Zephyr Pellerin" "smtp.gmail.com" 587)))


;; --------------------
;; RSS
;; --------------------

(setq nnrss-use-local t)

;; --------------------
;; Group Parameters
;; --------------------

(setq gnus-parameters
      '(("\\`nnrss:" 
         (display . all)
         (mm-discouraged-alternatives nil))))


;; --------------------
;; Miscellaneous configuration parameters
;; --------------------

;; Don't bother with the startup message
(setq gnus-inhibit-startup-message t
      ;; To save some time on exit. I don't use any other news reader.
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      ;; Commands should not attempt to go to the next unread article
      gnus-summary-goto-unread 'never
      ;; Inline images?
      mm-attachment-override-types '("image/.*")

      ;; allow retrieving images in HTML contents with the <img> tags
      mm-inline-text-html-with-images t

      ;; default directory for saving attachments
      mm-default-directory "~/Downloads/"

      ;; Don't check on startup for groups higher than 3.
      gnus-activate-level 3
      ;; Use EWW's text renderer
      mm-text-html-renderer 'shr
      ;; Fetch only part of the article if we can.  I saw this in someone
      ;; else's .gnus
      ;;;; gnus-read-active-file 'some
      ;; Is the only way I found to stop gnus to
      ;; - ask me "how many articles from" and
      ;; - show-me-all-my-mail-all-ways.
      gnus-large-newsgroup 'nil

      ;; preferred coding systems for encoding outgoing messages
      mm-coding-system-priorities '(utf-8
                                    iso-latin-9
                                    ;; ensure that utf-8 is chosen
                                    ;; over iso-latin-9 and latin-1,
                                    ;; if possible
                                    iso-latin-1)
      ;; unconditionally read the dribble file
      gnus-always-read-dribble-file t

      ;;  Gnus will only know about the groups in my `.newsrc' file
     gnus-active-file nil  ; speed-up
     
     ;; List our inactive groups
     gnus-group-list-inactive-groups t

      ;; Attachments
      ;; rewrite file names of MIME parts (delete control characters, delete
      ;; shell gotchas, handle evil white spaces)
      mm-file-name-rewrite-functions
      '(mm-file-name-delete-control
        mm-file-name-delete-gotchas
        mm-file-name-trim-whitespace
        mm-file-name-collapse-whitespace
        mm-file-name-replace-whitespace)
      )

;; --------------------
;; HTML Mail Display
;; --------------------

(setq gnus-html-cache-directory "/tmp/gnus"
      gnus-html-frame-width 80)
;; from Tassilo Horn, 17/7/14
(setq shr-color-visible-distance-min 10
      shr-color-visible-luminance-min 60)

;; Function to get list of links

;; --------------------
;; Window Configuration
;; --------------------
;; • Narrow left hand side occupied by group buffer.  Right hand side
;;   split between summary buffer (top one-sixth) and article buffer
;;   (bottom).
;;   +---+---------+
;;   | G | Summary |
;;   | r +---------+
;;   | o |         |
;;   | u | Article |
;;   | p |         |
;;   +---+---------+

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 25 (group 1.0))
               (vertical 1.0
                         (summary 0.16 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 25 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

(gnus-add-configuration
 '(server
   (horizontal 1.0
               (vertical 40 (server 1.0 point))
               (vertical 1.0 (browse 1.0)))))

(if window-system
    (setq
     ;; Summary Threading Marks
     gnus-sum-thread-tree-indent          " "
     gnus-sum-thread-tree-false-root      "☆ "
     gnus-sum-thread-tree-single-indent   "◎ "
     gnus-sum-thread-tree-root            "● "
     gnus-sum-thread-tree-vertical        "┃"
     gnus-sum-thread-tree-leaf-with-other "┣━❯ "
     gnus-sum-thread-tree-single-leaf     "┗━❯ "

     ;; Summary Mode Marks
     gnus-score-over-mark  ?↑   
     gnus-score-below-mark ?↓ 
     gnus-ticked-mark      ?⚑
     gnus-dormant-mark     ?⚐
     gnus-expirable-mark   ?♻
     gnus-read-mark        ?✓
     gnus-del-mark         ?✗
     gnus-killed-mark      ?☠
     gnus-replied-mark     ?⟲
     gnus-forwarded-mark   ?⤳
     gnus-cached-mark      ?☍
     gnus-recent-mark      ?★
     gnus-unseen-mark      ?✩
     gnus-unread-mark      ?✉))

;; --------------------
;; Formatting and Interface
;; --------------------
;; add buttons
(setq gnus-treat-buttonize t)
(setq gnus-treat-buttonize-head 'head)

;; do not display smileys as pictures
(setq gnus-treat-display-smileys nil)
(setq gnus-treat-fill-article 0)

;; (when (try-require 'auto-complete)
;;   (auto-complete-mode))


(defun my-change-smtp ()
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (addr fname server port) in smtp-accounts
          when (string-match addr from)
          do (setq user-mail-address addr
                   user-full-name fname
                   smtpmail-smtp-user addr
                   smtpmail-smtp-server server
                   smtpmail-smtp-service port))))

(defadvice smtpmail-via-smtp
    (before change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer (my-change-smtp)))

(setq message-send-mail-function 'smtpmail-send-it)

(setq gnus-posting-styles
      '(
        (".*"
        ;;  (name "zv")
        ;;  (address "zv@nxvr.org"))
        ;; (header "from" "zephyr.pellerin@gmail.com"
                (name "Zephyr Pellerin")
                (address "zephyr.pellerin@gmail.com")
                )))


;; Show Gravatar
(setq gnus-treat-mail-gravatar 'head)

(setq gnus-server-line-format "     {%(%h:%w%)} %s%a\n")

;; --------------------
;; Summary Mode
;; --------------------
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"                 ;; Status
       "%3{│%}" "%1{%d%}" "%3{│%}"   ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%B"
       "%s\n")
      ;; Adopt a previous line in for our false roots
      gnus-summary-make-false-root 'adopt)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

;; Add Keybindings ------------------------------------------
(defun zv/gnus-summary-mode-hook ()
  (gnus-define-keys gnus-summary-mode-map
    "J" gnus-summary-next-unread-article
    "K" gnus-summary-prev-unread-article
    "j" gnus-summary-next-article
    "k" gnus-summary-prev-article
    "\M-\C-j" gnus-summary-next-same-subject
    "\M-\C-k" gnus-summary-prev-same-subject
    "\C-j" gnus-summary-next-unread-subject
    "\C-k" gnus-summary-prev-unread-subject
    "/" gnus-summary-search-article-forward
    "?" gnus-summary-search-article-backward
    "P" gnus-summary-refer-parent-article
    "n" gnus-summary-repeat-search-article-forward
    "p" gnus-summary-repeat-search-article-backward
    "a" gnus-summary-post-news
    "!" gnus-summary-tick-article-forward
    "x" gnus-summary-mark-as-expirable
    "|" gnus-summary-pipe-output
    "q" gnus-summary-exit
    "Q" gnus-summary-exit-no-update
    "<" gnus-summary-beginning-of-article
    ">" gnus-summary-end-of-article
    "H" gnus-summary-toggle-header
    "x" gnus-summary-enter-digest-group)

  (gnus-define-keys (gnus-summary-limit-map "f" gnus-summary-mode-map)
    "/" gnus-summary-limit-to-subject
    "n" gnus-summary-limit-to-articles
    "b" gnus-summary-limit-to-bodies
    "h" gnus-summary-limit-to-headers
    "w" gnus-summary-pop-limit
    "s" gnus-summary-limit-to-subject
    "a" gnus-summary-limit-to-author
    "u" gnus-summary-limit-to-unread
    "m" gnus-summary-limit-to-marks
    "M" gnus-summary-limit-exclude-marks
    "v" gnus-summary-limit-to-score
    "*" gnus-summary-limit-include-cached
    "D" gnus-summary-limit-include-dormant
    "T" gnus-summary-limit-include-thread
    "d" gnus-summary-limit-exclude-dormant
    "t" gnus-summary-limit-to-age
    "x" gnus-summary-limit-to-extra
    "p" gnus-summary-limit-to-display-predicate
    "E" gnus-summary-limit-include-expunged
    "c" gnus-summary-limit-exclude-childless-dormant
    "C" gnus-summary-limit-mark-excluded-as-read
    "o" gnus-summary-insert-old-articles
    "N" gnus-summary-insert-new-articles
    "S" gnus-summary-limit-to-singletons
    "r" gnus-summary-limit-to-replied
    "R" gnus-summary-limit-to-recipient
    "A" gnus-summary-limit-to-address) 

  (gnus-define-keys (gnus-summary-thread-map "t" gnus-summary-mode-map)
    ;; Vim bindings
    "j" gnus-summary-next-thread
    "k" gnus-summary-prev-thread
    "v" gnus-summary-refer-thread
    "r" gnus-summary-refer-references
    ;; regular bindings
    "k" gnus-summary-kill-thread
    "E" gnus-summary-expire-thread
    "l" gnus-summary-lower-thread
    "i" gnus-summary-raise-thread
    "T" gnus-summary-toggle-threads
    "t" gnus-summary-rethread-current
    "^" gnus-summary-reparent-thread
    "\M-^" gnus-summary-reparent-children
    "s" gnus-summary-show-thread
    "S" gnus-summary-show-all-threads
    "h" gnus-summary-hide-thread
    "H" gnus-summary-hide-all-threads
    "n" gnus-summary-next-thread
    "p" gnus-summary-prev-thread
    "u" gnus-summary-up-thread
    "o" gnus-summary-top-thread
    "d" gnus-summary-down-thread
    "#" gnus-uu-mark-thread
    "\M-#" gnus-uu-unmark-thread)

  ;; Ensure our global bindings are not overridden
  (define-key gnus-summary-mode-map prev-buffer-key 'evil-window-prev)
  (define-key gnus-summary-mode-map next-buffer-key 'evil-window-next))

(add-hook 'gnus-summary-mode-hook 'zv/gnus-summary-mode-hook)

;; --------------------
;; Group Mode
;; --------------------
(setq gnus-group-line-format "%P%4y:%(%uG%) %M%S%p \n")

(defun gnus-user-format-function-G (_arg)
  (replace-regexp-in-string
   "\\(gwene\.\\|feedburner\.\\)?"
   ""
   (replace-regexp-in-string
    "\\(gwene.com\\|gmane\\)\.\\(comp.\\)?"
    ""
    gnus-tmp-qualified-group)))

;; • Keep track of the last time we opened this group.
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(defun zv/gnus-group-mode-hook ()
  (evil-set-initial-state 'gnus-group-mode 'motion)

  ;; • Define our keys
  (evil-define-key 'motion gnus-group-mode-map
    ;;"/"          'evil-search-forward
    ;; "?"         'evil-search-backward
    "r"            'gnus-topic-get-new-news-this-topic
    (kbd "RET")    'gnus-topic-select-group
    (kbd "M-RET")  'gnus-group-visible-select-group
    "="            'gnus-topic-select-group
    "\r"           'gnus-topic-select-group

    "\M-\r"        'gnus-group-quick-select-group
    "\M- "         'gnus-group-visible-select-group
    "f"            'gnus-group-jump-to-group
    "J"            'gnus-group-next-unread-group
    "K"            'gnus-group-prev-unread-group
    ;; "j"         'gnus-group-next-group
    ;; "k"         'gnus-group-prev-group
    "["            'gnus-topic-goto-previous-topic
    "]"            'gnus-topic-goto-next-topic
    ;;","          'gnus-group-best-unread-group
    ;;"."          'gnus-group-first-unread-group
    "u"            'gnus-group-unsubscribe-current-group
    "c"            'gnus-topic-catchup-articles

    ;; Topics
    [tab]          'gnus-topic-indent
    [(meta tab)]   'gnus-topic-unindent

    "C"            'gnus-group-catchup-current-all
    "\M-c"         'gnus-group-clear-data
    "l"            'gnus-group-list-groups
    "L"            'gnus-group-list-all-groups
    "m"            'gnus-group-mail
    "i"            'gnus-group-news
    "g"            'gnus-group-get-new-news
    "\M-g"         'gnus-group-get-new-news-this-group
    "R"            'gnus-group-restart
    "B"            'gnus-group-browse-foreign-server
    "b"            'gnus-group-check-bogus-groups
    "a"            'gnus-group-post-news
    ;; visual 
    ;; "x"         'gnus-group-kill-region
    "\C-c\C-x"     'gnus-group-expire-articles
    "\C-c\M-\C-x"  'gnus-group-expire-all-groups
    "q"            'gnus-group-exit
    "Q"            'gnus-group-quit
    "^"            'gnus-group-enter-server-mode
    "t"            'gnus-topic-mode
    "\M-&"         'gnus-group-universal-argument
    )

  (mapc
   (lambda (state)
     (evil-define-key state gnus-group-mode-map
       "o"     'gnus-group-mark-group
       "\M-o"  'gnus-group-unmark-group
       "s"     'gnus-group-sort-groups
       "M"     'gnus-topic-move-group
       "Sl"    'gnus-group-set-current-level
       "St"    'gnus-group-unsubscribe-current-group
       "Ss"    'gnus-group-unsubscribe-group
       "Sk"    'gnus-group-kill-group
       "Sy"    'gnus-group-yank-group
       "Sw"    'gnus-group-kill-region
       "S\C-k" 'gnus-group-kill-level
       "Sz"    'gnus-group-kill-all-zombies))
   '(motion normal visual))
  
  (evil-leader/set-key-for-mode 'gnus-group-mode
    "m/"   'gnus-group-make-nnir-group
    "msn" 'gnus-group-save-newsrc
    ;; Other
    "mkl" 'gnus-group-edit-local-kill
    "mkl" 'gnus-group-edit-global-kill
    "mgk" 'gnus-group-kill-group
    "mgy" 'gnus-group-yank-group
    ;; [w]ho?
    "mgW" 'gnus-group-list-killed
    ;; Group manipulation
    "mgm" 'gnus-group-make-group
    "mgE" 'gnus-group-edit-group
    "mge" 'gnus-group-edit-group-method
    "mgp" 'gnus-group-edit-group-parameters
    "mgv" 'gnus-group-add-to-virtual
    "mgD" 'gnus-group-enter-directory
    "mgr" 'gnus-group-rename-group
    "mgc" 'gnus-group-customize
    "mgz" 'gnus-group-compact-group
    "mgx" 'gnus-group-expunge-group
    "mgd" 'gnus-group-make-directory-group
    "mgh" 'gnus-group-make-help-group
    "mgu" 'gnus-group-make-useful-group
    "mgl" 'gnus-group-nnimap-edit-acl
    "mgV" 'gnus-group-make-empty-virtual
    "mgf" 'gnus-group-make-doc-group
    "mgw" 'gnus-group-make-web-group
    "mgg" 'gnus-group-make-nnir-group
    "mgR" 'gnus-group-make-rss-group
    "mgM" 'gnus-group-read-ephemeral-group
    "mg<backspace>" 'gnus-group-delete-group

    ;; sieve
    "mvu" 'gnus-sieve-update
    "mvg" 'gnus-sieve-generate

    ;; Listing of groups
    "mak" 'gnus-group-list-killed
    "maz" 'gnus-group-list-zombies
    "mas" 'gnus-group-list-groups
    "mau" 'gnus-group-list-all-groups
    "maA" 'gnus-group-list-active
    "maa" 'gnus-group-apropos
    "mad" 'gnus-group-description-apropos
    "mam" 'gnus-group-list-matching
    "maM" 'gnus-group-list-all-matching
    "mal" 'gnus-group-list-level
    "mac" 'gnus-group-list-cached
    "ma?" 'gnus-group-list-dormant
    "ma!" 'gnus-group-list-ticked

    "mss" 'gnus-topic-sort-groups
    "msa" 'gnus-topic-sort-groups-by-alphabet
    "msu" 'gnus-topic-sort-groups-by-unread
    "msl" 'gnus-topic-sort-groups-by-level
    "mse" 'gnus-topic-sort-groups-by-server
    "msv" 'gnus-topic-sort-groups-by-score
    "msr" 'gnus-topic-sort-groups-by-rank
    "msm" 'gnus-topic-sort-groups-by-method

    "mtn" 'gnus-topic-create-topic
    "mt#" 'gnus-topic-mark-topic
    "mt\M-#" 'gnus-topic-unmark-topic
    "mtm" 'gnus-topic-move-group
    "mtD" 'gnus-topic-remove-group
    "mtc" 'gnus-topic-copy-group
    "mth" 'gnus-topic-hide-topic
    "mts" 'gnus-topic-show-topic
    "mtj" 'gnus-topic-jump-to-topic
    "mtM" 'gnus-topic-move-matching
    "mtC" 'gnus-topic-copy-matching
    "mtr" 'gnus-topic-rename

    ;;"mt\M-p" 'gnus-topic-goto-previous-topic
    ;;"mt\M-n" 'gnus-topic-goto-next-topic
    ;;"mt\C-i" 'gnus-topic-indent
    ;;"mt\t" 'gnus-topic-indent
    ;;"mt\177" 'gnus-topic-delete
    "mtH" 'gnus-topic-toggle-display-empty-topics)
 
  (guide-key/add-local-guide-key-sequence "G")
  (guide-key/add-local-guide-key-sequence "T")
  (guide-key/add-local-guide-key-sequence "S")
  (guide-key/add-local-guide-key-sequence "J")

  (define-key gnus-group-mode-map "z" nil)
  (define-key gnus-group-mode-map prev-buffer-key 'evil-window-prev)
  (define-key gnus-group-mode-map next-buffer-key 'evil-window-next)

  ;; • Ensure our group mode automatically adjusts to `Topic'
  (gnus-topic-mode))

(add-hook 'gnus-group-mode-hook 'zv/gnus-group-mode-hook)

;; --------------------
;; Topic Mode
;; --------------------
(setq gnus-topic-indent-level 2
      gnus-topic-line-format "%i[ %(%{%n%}%) ]%v\n"
      )

;; --------------------
;; Article Mode
;; --------------------
(setq gnus-visible-headers
      "^From:\\|^To:\\|^Subject:\\|^Date:\\|^User-Agent:\\|^X-Mailer:"
      gnus-summary-display-arrow t
      gnus-topic-indent-level 1
      gnus-group-uncollapsed-levels 2)

;; format specification for the article mode line
(setq gnus-article-mode-line-format "%S%m")
(evil-set-initial-state 'gnus-article-mode-map 'motion )

(defun zv/gnus-article-mode-hook ()
  (evil-set-initial-state 'gnus-article-mode 'motion)

  (evil-set-initial-state 'gnus-group-mode 'motion)

  (evil-leader/set-key-for-mode gnus-article-mode-map
    "maf" 'mml-attach-file
    "ms"  'gnus-article-edit-done
    "mQ"  'gnus-article-edit-done)

  (evil-define-key 'motion gnus-article-mode-map
    "gt"    'message-goto-to
    "go"    'message-goto-from
    "gb"    'message-goto-bcc
    "gc"    'message-goto-cc
    "gs"    'message-goto-subject
    "gr"    'message-goto-reply-to
    "gn"    'message-goto-newsgroups
    "gf"    'message-goto-followup-to
    "gm"    'message-goto-mail-followup-to
    "gu"    'message-goto-summary
    "gi"    'message-insert-or-toggle-importance
    "ga"    'message-generate-unsubscribed-mail-followup-to
    ;; Link browsing
    "n"   'shr-next-link
    "p"   'shr-previous-link

    "gb"    'message-goto-body
    "gi"    'message-goto-signature

    "Mt"    'message-insert-to
    "Mn"    'message-insert-newsgroups

    "So"    'message-sort-headers
    "de"    'message-elide-region
    "dz"    'message-kill-to-signature

    "\M-\r" 'message-newline-and-reformat
    "\t"    'message-tab))

(add-hook 'gnus-article-mode-hook 'zv/gnus-article-mode-hook)

;; --------------------
;; Message Mode
;; --------------------

;; Server Browse Mode
(evil-set-initial-state 'message-mode 'normal)

(defun zv/message-mode-hook ()
  ;; Initialize BBDB
  ;; (bbdb-initialize 'message)
  ;; (bbdb-initialize 'gnus)
  ;; (local-set-key "<TAB>" 'bbdb-complete-name)

  ;; Setup formatting
  (setq fill-column 72)
  (turn-on-auto-fill)

  ;; tab completion for alias in `.mailrc'
  (local-set-key (kbd "<M-tab>") 'mail-abbrev-complete-alias)

  ;; enable automatic word-wrap when composing messages
  (setq-default fill-column 80)
  (auto-fill-mode)

  ;; turn on the Org mode table editor (in emails)
  (turn-on-orgtbl)

  ;; turn on (the enhanced version of) orgstruct-mode
  (turn-on-orgstruct++)

  ;; ;; make `orgstruct-hijacker-command-22' rebind `M-q' to a message
  ;; ;; specific function to fill a paragraph
  ;; (setq fill-paragraph-function 'org-fill-paragraph)

  (when (try-require 'org-footnote)
    ;; default style used for footnoting is local to the Message being
    ;; written
    (set (make-local-variable 'org-footnote-auto-label) 'plain)

    ;; no tag marking the beginning of footnote section
    (set (make-local-variable
          'org-footnote-tag-for-non-org-mode-files) nil))

  (evil-define-key 'normal message-mode-map
    "gt" 'message-goto-to
    "go" 'message-goto-from
    "gb" 'message-goto-body
    "gw" 'message-goto-fcc
    "gc" 'message-goto-cc
    "gz" 'message-goto-signature
    "gs" 'message-goto-subject
    "gr" 'message-goto-reply-to
    "gn" 'message-goto-newsgroups
    "gd" 'message-goto-distribution
    "gf" 'message-goto-followup-to
    "gm" 'message-goto-mail-followup-to
    "gk" 'message-goto-keywords
    "gu" 'message-goto-summary
    "gi" 'message-insert-or-toggle-importance
    "de" 'message-kill-address
    "dr" 'message-elide-region
    "dz" 'message-kill-to-signature
    "\M-\r" 'message-newline-and-reformat
    "\M-n" 'message-display-abbrev
    "\C-c\C-f\C-a" 'message-generate-unsubscribed-mail-followup-to)

  (evil-leader/set-key-for-mode 'message-mode
    ;; [m]essage [d]eclare
    "mdr" 'message-mark-inserted-region
    "mdf" 'message-mark-insert-file
    ;; [m]essage [i]nsert
    "mit" 'message-insert-to
    "miw" 'message-insert-wide-reply
    "min" 'message-insert-newsgroups
    "mil" 'message-to-list-only
    "mix" 'message-insert-expires
    "mis" 'message-insert-signature
    "mih" 'message-insert-headers
    "miu" 'message-insert-or-toggle-importance
    "mid" 'message-insert-disposition-notification-to
    ;; [m]essage [f]uck
    "mfx" 'message-cross-post-followup-to
    "mft" 'message-reduce-to-to-cc
    "mfa" 'message-add-archive-header
    ;; [m]essage [y]ank
    "myy" 'message-yank-original
    "myb" 'message-yank-buffer
    "mms" 'message-sort-headers

    ;; Direct message items
    "ms" 'message-send-and-exit
    "mq" 'message-kill-buffer
    "mmr" 'message-rename-buffer
    "mms" 'message-send
    "mmd" 'gnus-delay-article
    "mmb" 'message-dont-send)
  )

;; --------------------
;; Server Mode
;; --------------------

;; Server Browse Mode
(evil-set-initial-state 'gnus-browse-mode 'motion)

(eval-after-load 'gnus-browse-mode
  '(zv/define-keymap gnus-browse-mode-map
                     '(("j" . gnus-browse-next-group)
                       ("k" . gnus-browse-prev-group)
                       ("u" . gnus-browse-unsubscribe-current-group)
                       ("d" . gnus-browse-describe-group)
                       ("q" . gnus-browse-exit))))


;; --------------------
;; Caching
;; --------------------
;; use the cache
(setq gnus-use-cache t)

;; ;; local cache
(setq gnus-cache-directory "/tmp/gnus")

;; entering of articles from the cache
(setq gnus-cache-enter-articles '(ticked dormant unread read))

;; removing of articles from the cache
(setq gnus-cache-remove-articles nil)

;; cache your nnimap groups
(setq gnus-cacheable-groups "^nnimap")

;; avoid caching your nnml and nnfolder groups
(setq gnus-uncacheable-groups "^nnml\\|^nnfolder\\|^nnrss")

;; --------------------
;; Message Encryption
;; --------------------
(use-package epg-config
  :init
  ;; Sign our messages with an attachment
  (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
  :config
  (setq
   ;; Use epg rather than pgg
   mml2015-use                       'epg
   ;; Who I am
   epg-user-id                       "F6F2D0445DC172F8"
   mml2015-signers                   '("F6F2D0445DC172F8")
   mml2015-verbose                   t
   mml2015-encrypt-to-self           t
   ;; Don't skip key validation
   mml2015-always-trust              nil
   mml2015-cache-passphrase          t
   mml2015-passphrase-cache-expiry   65535
   mml2015-sign-with-sender          t

   ;; Reply-in-kind to encrypted/signed message 
   gnus-message-replyencrypt         t
   gnus-message-replysign            t
   gnus-message-replysignencrypted   t
   gnus-treat-x-pgp-sig              t

   ;; Don't ask for which key should be used -- just use the default
   mm-sign-option                    nil
   mm-encrypt-option                 nil
   ;; Always verify & decrypt any signed/encrypted messages
   mm-verify-option                  'always
   mm-decrypt-option                 'always
   
   gnus-buttonized-mime-types '("multipart/alternative"
                                "multipart/encrypted"
                                "multipart/signed")
   epg-debug t))

;; Add hook to automatically recieve keys -------------------
(defun gnus-article-receive-epg-keys ()
  "Fetch unknown keys from a signed message."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (save-excursion
      (goto-char (point-min))
      (if
          (re-search-forward "\\[\\[PGP Signed Part:No public key for \\([A-F0-9]\\{16,16\\}\\) created at "
                             nil 'noerror)
          (shell-command (format "gpg --keyserver %s --recv-keys %s"
                                 "pgp.mit.edu"
                                 (match-string 1)))
        (message "No unknown signed parts found.")))))

(add-hook
 'gnus-startup-hook
 (lambda nil
   (define-key gnus-article-mode-map (kbd "C-c k") 'gnus-article-receive-epg-keys)
   (define-key gnus-summary-mode-map (kbd "C-c k") 'gnus-article-receive-epg-keys)))

;; --------------------
;; Forwarding
;; --------------------

;; delimiter inserted before forwarded messages
(setq message-forward-start-separator "-----Original Message-----\n")

;; delimiter inserted after forwarded messages
(setq message-forward-end-separator "\n")

;; subject of article with `Fwd:' prepended to it, for forwarded messages
(setq message-make-forward-subject-function 'message-forward-subject-fwd)

;; forwarded messages will just be copied inline to the new message
(setq message-forward-as-mime nil)

;; --------------------
;; Forwarding
;; --------------------
(use-package mailcap
  :defer t
  :config
  (progn
    (add-to-list 'mailcap-mime-extensions
                 '(".doc" . "application/msword"))
    (add-to-list 'mailcap-mime-extensions
                 '(".ppt" . "application/vnd.ms-powerpoint"))))
