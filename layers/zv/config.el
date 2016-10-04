;; -*- mode: emacs-lisp -*-

;; Emacs global configuration
(defconst zv-configuration-layer-directory (car dotspacemacs-configuration-layer-path)
  "zv contribution layer base directory.")

(setq-default
 spacemacs-repository "emacs.d"
 spacemacs-repository-owner "zv"
 ;; ERC
 zv-erc-directory (expand-file-name (concat user-emacs-directory ".erc/"))
 ignored-irc-commands '("JOIN" "PART" "QUIT" "NICK" "AWAY")
 user-mail-address                "zv@nxvr.org"

 ;; default browser used
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "google-chrome"

 ;; GDB configuration
 gdb-many-windows t
 gdb-show-main t

 ;; hippie expand is dabbrev expand on steroids
 hippie-expand-try-functions-list '(try-expand-dabbrev
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-complete-file-name-partially
                                    try-complete-file-name
                                    try-expand-all-abbrevs
                                    try-expand-list
                                    try-expand-line
                                    try-complete-lisp-symbol-partially
                                    try-complete-lisp-symbol))

(defvar zv-whitelist '() "List of helm buffers in which to show dots.")

;; encrypt hook ------------------------------------------------------------------
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
(if (and (file-executable-p "/usr/sbin/sendmail")
         (require 'sendmail nil t))
    (setq message-send-mail-function 'sendmail-send-it
          send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it))
