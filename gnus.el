(setq smtpmail-auth-credentials (expand-file-name "~/.authinfo"))

;; Mail configuration ----------------------------------------------------------
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnimap-inbox "gmail")))

(setq gnus-secondary-select-methods
      '((nnimap "nxvr"
                (nnimap-address "mail.nxvr.org")
                (nnimap-server-port 143)
                (nnimap-inbox "nxvr")
                (nnimap-stream network))
        (nntp "news.gmane.org")))

;; Multiple SMTP servers
(setq smtp-accounts
      '(("zv@nxvr.org" "zephyr" "mail.nxvr.org" 10025)
        ("zephyr.pellerin@gmail.com" "Zephyr Pellerin" "smtp.gmail.com" 587)))

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

;; Add Keybindings ------------------------------------------
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (define-key gnus-summary-mode-map "j" 'gnus-summary-next-article)
            (define-key gnus-summary-mode-map "k" 'gnus-summary-prev-article)
            ;; Ensure our global bindings are not overridden
            (define-key gnus-summary-mode-map prev-buffer-key 'evil-window-prev)
            (define-key gnus-summary-mode-map next-buffer-key 'evil-window-next)
            ))

(add-hook 'gnus-group-mode-hook
          (lambda ()
            (define-key gnus-group-mode-map "j" 'gnus-group-next-group)
            (define-key gnus-group-mode-map "k" 'gnus-group-prev-group)
            (define-key gnus-group-mode-map "f" 'gnus-group-jump-to-group)
            ;; Ensure our global bindings are not overridden
            (define-key gnus-group-mode-map prev-buffer-key 'evil-window-prev)
            (define-key gnus-group-mode-map next-buffer-key 'evil-window-next)
            ))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (evil-leader/set-key-for-mode 'gnus-article-mode
              "maf" 'mml-insert-part
              ;; Attach directory
              "mad" 'gnus-dired-attach
              "mep" 'mml-secure-message-encrypt-pgp
              "mer" 'mml-unsecure-message)))


(require 'bbdb)
(bbdb-initialize)
(add-hook 'gnus-Startup-hook 'bbdb-insinuate-gnus)

(setq gnus-group-list-inactive-groups t)

(setq
 message-use-mail-followup-to "use" ;; Always follow up with an appropriate MFT message
 message-subscribed-address-functions '(gnus-find-subscribed-addresses))

;; formatting and interface -----------------------------------------------------
(setq gnus-visible-headers "\\|^User-Agent:\\|^X-Mailer:")
(setq gnus-group-sort-function 'gnus-sort-by-rank)
(setq gnus-thread-sort-functions '((not gnus-thread-sort-by-date)))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)

(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)

(defun gnus-user-format-function-G (_arg)
  (replace-regexp-in-string "\\(gwene.com\\|gmane\\)\.\\(comp.\\)?" "" gnus-tmp-qualified-group))

(setq gnus-group-line-format "%P%5y:%B%(%uG%) %M%S%p \n")

(setq gnus-summary-line-format        "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
      gnus-summary-dummy-line-format  "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
      gnus-visible-headers            "^From:\\|^To:\\|^Subject:\\|^Date:\\|^User-Agent:\\|^X-Mailer:"
      gnus-topic-indent-level 1
      gnus-group-uncollapsed-levels 2
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "┏● " 
      gnus-sum-thread-tree-false-root " ○ "
      gnus-sum-thread-tree-single-indent " ● "
      gnus-sum-thread-tree-leaf-with-other "┣━━❯ " 
      gnus-sum-thread-tree-vertical "┃"
      gnus-sum-thread-tree-single-leaf "┗━━❯ ")

;; Group by topic mode
;; (setq nntp-nov-is-evil t)

;; Use the tree-mode buffer
(setq gnus-use-trees nil
      gnus-tree-minimize-window nil
      gnus-generate-tree-function 'gnus-generate-horizontal-tree)

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

;; A simple layout ----------------------------------------
;; +---+---------+
;; | G | Summary |
;; | r +---------+
;; | o |         |
;; | u | Article |
;; | p |         |
;; +---+---------+

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 30 (group 1.0))
               (vertical 1.0
                         (summary 0.2 point)
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 30 (group 1.0))
               (vertical 1.0 (summary 1.0 point)))))

;; Show some additional headers
(setq gnus-extra-headers '(To X-NextAction X-Waiting))
(setq nnmail-extra-headers gnus-extra-headers)

; Inline images?
(setq mm-attachment-override-types '("image/.*"))

(defun zv-email-formatting-init ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'zv-email-formatting-init)

;; Topic Group Mode -------------------------------------------
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; GPG encryption ---------------------------------------------
(require 'epg-config)
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
 epg-debug t)

;; Sign our messages with an attachment
(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
