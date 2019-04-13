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
 ;; hippie expand is dabbrev expand on steroids
 hippie-expand-try-functions-list '(try-expand-dabbrev
                                    try-expand-dabbrev-visible
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

(if (and (file-executable-p "/usr/sbin/sendmail")
         (require 'sendmail nil t))
    (setq message-send-mail-function 'sendmail-send-it
          send-mail-function 'sendmail-send-it)
  (setq message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it))
