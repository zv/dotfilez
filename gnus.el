;; Mail configuration ----------------------------------------------------------
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnimap-inbox "gmail")))
(setq gnus-secondary-select-methods
      '(
        (nnimap "nxvr"
                (nnimap-address "mail.nxvr.org")
                (nnimap-server-port 143)
                (nnimap-inbox "nxvr")
                (nnimap-stream network))
        (nntp "news.gmane.org")
        )
      )

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq gnus-parameters
      '((".*"
         (display . all)
         (posting-style
          (name "Zephyr Pellerin")
          (address "zephyr.pellerin@gmail.com")
          ))
        ("news:.*"
         (posting-style
          (name "zv")
          (address "zv@nxvr.org")))
        ))

;; Add Keybindings ------------------------------------------
(define-key gnus-summary-mode-map "j" 'gnus-summary-next-article)
(define-key gnus-summary-mode-map "k" 'gnus-summary-prev-article)
(define-key gnus-group-mode-map "j" 'gnus-group-next-group)
(define-key gnus-group-mode-map "k" 'gnus-group-prev-group)
(define-key gnus-group-mode-map "f" 'gnus-group-jump-to-group)
(evil-leader/set-key-for-mode 'gnus-article-mode
  "am" 'mml-insert-part
  "dem" 'mml-secure-message-encrypt-pgpmime
  "dep" 'mml-secure-message-encrypt-pgp
  "der" 'mml-unsecure-message)

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

(defun format-group-name ())
(setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
      gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
      gnus-visible-headers "^From:\\|^To:\\|^Subject:\\|^Date:\\|^User-Agent:\\|^X-Mailer:"
      gnus-topic-indent-level 1
      gnus-group-uncollapsed-levels 2
      gnus-group-line-format "%S %(%~(cut-left 7)-25,25c%) %6y\n"
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

;; Add hook to automatically recieve keys --------------------------------------
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

(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 25 (group 1.0))
               (vertical 1.0
                         (horizontal 0.2
                                     (summary 1.0 point)
                                     (tree 0.2))
                         
                         (article 1.0)))))

(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 25 (group 1.0))
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

;; Topic Group Mode -------------------------------------------------------------
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; GPG encryption ---------------------------------------------------------------
(require 'epg-config)
(setq mml2015-use 'epg
      mml2015-verbose                   t
      mml2015-encrypt-to-self           t
      mml2015-always-trust              nil
      mml2015-cache-passphrase          t
      mml2015-passphrase-cache-expiry   '36000
;;      mml2015-sign-with-sender          t
      
      gnus-message-replyencrypt         t
      gnus-message-replysign            t
      gnus-message-replysignencrypted   t
      gnus-treat-x-pgp-sig              t
      
      mm-sign-option                    'guided
      mm-encrypt-option                 'guided
      mm-verify-option                  'always
      mm-decrypt-option                 'always
      
      gnus-buttonized-mime-types
      '("multipart/alternative"
        "multipart/encrypted"
        "multipart/signed")

      epg-debug t)

;; Automatically sign my messages
(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime)
