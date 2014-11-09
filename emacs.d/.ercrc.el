(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)

;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; open query buffers in the current window
(setq erc-query-display 'buffer)

(setq erc-log-channels-directory "~/.erc/logs/")

(erc-track-mode t)

;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

; Don't shorten the names of channels
(setq erc-track-shorten-function nil)

;; exclude boring stuff from tracking
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#xmonad"
                                     "#tahoe-lafs"
                                     "#gdb"
                                     "#evil-mode"
                                     "#pctf"
                                     "#r_netsec"
                                     "#go-nuts"
                                     "#emacs"
                                     "#scheme"
                                     "#postgresql"
                                     "#zsh"
                                     "#cat-v"
                                     "#noisebridge"
                                     "#radare"
                                     "#emacs"
                                     )))
