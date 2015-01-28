(require 'gnus)
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

;; ---------------------------------------------------------------------------
;; Miscellaneous configuration parameters
;; ---------------------------------------------------------------------------

;; Don't bother with the startup message
(setq gnus-inhibit-startup-message t
      ;; To save some time on exit. I don't use any other news reader.
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      ;; Inline images?
      mm-attachment-override-types '("image/.*")
      ;; Don't check on startup for groups higher than 3.
      gnus-activate-level 3
      ;; Use EWW's text renderer
      mm-text-html-renderer 'shr
      ;; Fetch only part of the article if we can.  I saw this in someone
      ;; else's .gnus
      gnus-read-active-file 'some
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

;; ---------------------------------------------------------------------------
;; HTML Mail Display
;; ---------------------------------------------------------------------------

(setq gnus-html-cache-directory "/tmp/gnus"
      gnus-html-frame-width 80)
;; from Tassilo Horn, 17/7/14
(setq shr-color-visible-distance-min 10
      shr-color-visible-luminance-min 60)

;; Function to get list of links

;; ---------------------------------------------------------------------------
;; Window Configuration
;; ---------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------
;; Summary Mode
;; ---------------------------------------------------------------------------
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"                 ;; Status
       "%3{│%}" "%1{%d%}" "%3{│%}"   ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n")
      ;; Adopt a previous line in for our false roots
      gnus-summary-make-false-root 'adopt)

;; Add Keybindings ------------------------------------------
(defun zv/gnus-summary-mode-hook  ()
  (define-key gnus-summary-mode-map "j" 'gnus-summary-next-article)
  (define-key gnus-summary-mode-map "k" 'gnus-summary-prev-article)
  ;; Ensure our global bindings are not overridden
  (define-key gnus-summary-mode-map prev-buffer-key 'evil-window-prev)
  (define-key gnus-summary-mode-map next-buffer-key 'evil-window-next))

(add-hook 'gnus-summary-mode-hook 'zv/gnus-summary-mode-hook)

;; ---------------------------------------------------------------------------
;; Group Mode
;; ---------------------------------------------------------------------------
(setq gnus-group-line-format "%P%5y:%B%(%uG%) %M%S%p \n")

(defun gnus-user-format-function-G (_arg)
  (replace-regexp-in-string "\\(gwene.com\\|gmane\\)\.\\(comp.\\)?" "" gnus-tmp-qualified-group))

;; • Keep track of the last time we opened this group.
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)


(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun zv/gnus-group-mode-hook ()
  ;; • Define our keys
  (zv/define-keymap 'gnus-group-mode-map zv-gnus-group-keymap)

  (guide-key/add-local-guide-key-sequence "G")
  (guide-key/add-local-guide-key-sequence "T")

  (define-key gnus-group-mode-map "z" nil)
  (define-key gnus-group-mode-map prev-buffer-key 'evil-window-prev)
  (define-key gnus-group-mode-map next-buffer-key 'evil-window-next)

  ;; • Ensure our group mode automatically adjusts to `Topic'
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(add-hook 'gnus-group-mode-hook 'zv/gnus-group-mode-hook)

;; ---------------------------------------------------------------------------
;; Article Mode
;; ---------------------------------------------------------------------------
(require 'gnus-art)

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (evil-leader/set-key-for-mode 'gnus-article-mode
              "maf" 'mml-insert-part
              ;; Attach directory
              "mad" 'gnus-dired-attach
              "mep" 'mml-secure-message-encrypt-pgp
              "mer" 'mml-unsecure-message)))

(evil-leader/set-key-for-mode 'gnus-group-mode "ms" 'server)

;; Prefixes
;; t - Topics

;; formatting and interface -----------------------------------------------------
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))

(setq gnus-visible-headers
      "^From:\\|^To:\\|^Subject:\\|^Date:\\|^User-Agent:\\|^X-Mailer:"
      gnus-summary-display-arrow t
      gnus-topic-indent-level 1
      gnus-group-uncollapsed-levels 2)



;; add buttons
(setq gnus-treat-buttonize t)
(setq gnus-treat-buttonize-head 'head)

;; do not display smileys as pictures
(setq gnus-treat-display-smileys nil)
(setq gnus-treat-fill-article 0)

;; format specification for the article mode line
(setq gnus-article-mode-line-format "%S%m")

;; make `C-c C-f' active from within messages
;;(define-key gnus-article-mode-map
;;  (kbd "C-c C-f") 'gnus-summary-mail-forward)

;; ---------------------------------------------------------------------------
;; Message Mode
;; ---------------------------------------------------------------------------
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

  ;; Turn on message mode bindings
  (evil-leader/set-key-for-mode 'gnus-message-mode
    "mb" 'bbdb)

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
  )

;; ---------------------------------------------------------------------------
;; Server Mode
;; ---------------------------------------------------------------------------

;; Server Browse Mode
(evil-set-initial-state 'gnus-browse-mode 'motion)

(eval-after-load 'gnus-browse-mode
  '(zv/define-keymap gnus-browse-mode-map
                     '(("j" . gnus-browse-next-group)
                       ("k" . gnus-browse-prev-group)
                       ("u" . gnus-browse-unsubscribe-current-group)
                       ("d" . gnus-browse-describe-group)
                       ("q" . gnus-browse-exit))))


;; ---------------------------------------------------------------------------
;; Caching
;; ---------------------------------------------------------------------------
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
(setq gnus-uncacheable-groups "^nnml\\|^nnfolder")

;; ---------------------------------------------------------------------------
;; Message Encryption
;; ---------------------------------------------------------------------------
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


;; ---------------------------------------------------------------------------
;; Forwarding
;; ---------------------------------------------------------------------------

;; delimiter inserted before forwarded messages
(setq message-forward-start-separator "-----Original Message-----\n")

;; delimiter inserted after forwarded messages
(setq message-forward-end-separator "\n")

;; subject of article with `Fwd:' prepended to it, for forwarded messages
(setq message-make-forward-subject-function 'message-forward-subject-fwd)

;; forwarded messages will just be copied inline to the new message
(setq message-forward-as-mime nil)
