(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)

(add-hook 'erc-mode-hook
          (lambda ()
            (setq mode-line-format `("%e"
                                     mode-line-client
                                     mode-line-frame-identification
                                     mode-line-buffer-identification
                                     "  "
                                     mode-line-misc-info
                                     mode-line-end-spaces))
            ))

;; keybindings -------------------------------------------
(evil-add-hjkl-bindings erc-mode-map 'motion
  "0" 'erc-bol
  )
;; Configure some leader keys for erc
(evil-leader/set-key-for-mode 'erc-mode
  "ej" 'erc-join-channel
  "el" 'erc-go-to-log-matches-buffer
  "es" 'erc-save-buffer-in-logs
  )

;; configure various ERC settings -----------------------------------------------
(setq
 erc-nick "zv"
 erc-user-full-name "zetavolt"
 erc-email-userid "zv@nxvr.org"
 erc-auto-query 'bury
 erc-prompt ">"
 ;; Kill buffers for channels after /part
 erc-kill-buffer-on-part t
 ;; Use mirc colors
 erc-interpret-mirc-color t
 ;; Use auth-source passwords
 erc-prompt-for-password nil
 ;; Kill buffers for private queries after quitting the server
 erc-kill-queries-on-quit t
 ;; Kill buffers for server messages after quitting the server
 erc-kill-server-buffer-on-quit t
 ;; open query buffers in the current window
 erc-query-display 'buffer
 erc-server-coding-system '(utf-8 . utf-8))

(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)


;; erc matching -----------------------------------------------------------------
(require 'erc-match)
(setq erc-keywords '("zephyr" "zv"))
(erc-match-mode)

;; logging ----------------------------------------------------------------------
(setq erc-log-insert-log-on-open nil
      erc-log-channels t
      erc-log-channels-directory "~/.emacs.d/erc/logs" 
      erc-save-buffer-on-part t
      erc-hide-timestamps nil)

;; ring mode --------------------------------------------------------------------
(require 'erc-ring)
(erc-ring-mode t)

;; truncate buffers -------------------------------------------------------------
(setq erc-max-buffer-size 20000)
(erc-truncate-mode t)

;; tracking mode ----------------------------------------------------------------
(setq
 ;; Don't shorten the names of channels
 erc-track-shorten-function nil 
 ;; exclude boring stuff from tracking
 erc-track-exclude-types (cons ignored-irc-commands '("324" "329" "332" "333" "353" "477"))
 erc-hide-list           ignored-irc-commands
 erc-track-exclude-types ignored-irc-commands)
(erc-track-mode t)

;; autojoin channels ------------------------------------------------------------
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#cat-v"
                                     "#elixir-lang" 
                                     "#erlang"
                                     "#haskell" 
                                     "#postgresql" 
                                     "#pwning"
                                     "#noisebridge"
                                     "#gdb"
                                     "##kernel"
                                     "#reactjs" 
                                     "##re"
                                     "#radare"
                                     "#stackvm"
                                     "#zsh"
                                     "#xmonad"
                                     "#emacs"
                                     "#scheme")
                                    ("irc.mozilla.org"
                                     "#rust"
                                     "#rust-internals")
                                    ("irc.oftc.net"
                                     "#tor")))
