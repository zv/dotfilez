;; Empty the definition of some spacemacs init methods
(defun spacemacs/goto-link-line ())
(defun spacemacs//insert-banner ())

(require 'mm-url) ; to include mm-url-decode-entities-string

;; erc
(defun erc-connect ()
  "Connect to IRC."
  (interactive)
  ;; disable powerline for ERC ----------------------------
  (erc :server "irc.freenode.net" :port 6667 :nick "zv")
  (erc :server "irc.mozilla.org" :port 6667 :nick "zv")
  (erc :server "irc.oftc.net" :port 6667 :nick "zv"))


(defun add-semicolon-to-end-of-line ()
    "Unsurprisingly, this adds a semicolon to the end of the line"
    (interactive)
    (save-excursion (end-of-line) (insert ";")))

(defun org-path (file)
        "Returns the full path of an org directory file"
        (expand-file-name (concat org-directory "/" file)))

(defun zv/define-keymap (mode-map keymap)
  (mapc (lambda (binding)
          (define-key mode-map (car binding) (cdr binding)))
        keymap))

(defun zv/declare-prefix-for-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let ((command (intern (concat spacemacs/prefix-command-string name))))
    (define-prefix-command command)
    (evil-leader/set-key-for-mode mode prefix command)))

(defun zv/enlarge-window-by-dominant-dimension (magnitude)
  "Enlarge the current window by height if vertically split, or width otherwise"
  (cond ((window-full-width-p) (enlarge-window magnitude))
        ((window-full-height-p) (enlarge-window-horizontally magnitude))
        (t (enlarge-window (/ magnitude 2)))))

(defun zv/tile-split-window ()
  "If our current window width / height is greater than 1.68, split vertically"
  (interactive)
  (let* ((window-ratio (/ (float (window-pixel-width)) (float (window-pixel-height))))
         (golden-ratio (/ (+ 1 (sqrt 5)) 2)))
    (if (> window-ratio golden-ratio)
        (evil-window-vsplit)
      (evil-window-split))))

(defun zv/join-up ()
  "hacky way to join parent's lines"
  (interactive)
  (save-excursion
    (progn
      (previous-line 2)
      (evil-join (point) (+ 1 (point))))))

(defun zv/scroll-up-one-line ()
  (interactive)
  (scroll-up-line 1))

(defun zv/scroll-down-one-line ()
  (interactive)
  (scroll-down-line 1))

;; Monkeypatch some spacemacs internal window positioning
(defun spacemacs/shrink-window-horizontally (delta)
  "Wrap `spacemacs/shrink-window-horizontally'."
  (interactive "p") (shrink-window 10 t))

(defun spacemacs/shrink-window (delta)
  "Wrap `spacemacs/shrink-window'."
  (interactive "p") (shrink-window 5))

(defun spacemacs/enlarge-window (delta)
  "Wrap `spacemacs/enlarge-window'."
  (interactive "p") (enlarge-window 5))

(defun spacemacs/enlarge-window-horizontally (delta)
  "Wrap `spacemacs/enlarge-window-horizontally'."
  (interactive "p") (enlarge-window 10 t))


;; Org Mode
;; --------

(defun clever-insert-item ()
  "Clever insertion of org item."
  (if (not (org-in-item-p))
      (insert "\n")
    (org-insert-item)))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

;; Sets an org-mode link's default text to be that of the page's title
(defun zv/org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (read-string "URL: "))
         (title (zv//get-html-title-from-url url)))
    (if title
        (org-insert-link nil url title)
      (org-insert-link))))

(defun zv//get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))


;; Restart `tern-mode`
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))


;; advice functions for not showing dots
(defun zv/whitelistedp ()
  (member (with-helm-buffer (buffer-name)) zv-whitelist))

(defun zv/helm-ff-filter-candidate-one-by-one (fcn file)
  (when (or (zv/whitelistedp)
            (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
    (funcall fcn file)))

(defun zv/helm-file-completion-source-p (&rest args) t)

(defun zv/helm-find-files-up-one-level (fcn &rest args)
  (prog2
      (advice-add 'helm-file-completion-source-p
                  :around 'zv/helm-file-completion-source-p)
      (apply fcn args)
    (advice-remove 'helm-file-completion-source-p
                   'zv/helm-file-completion-source-p)))


(defun sort-sexps-by-cadr (reverse beg end)
  "Sort by particular sexp field (in this casse, the cadr)"
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (sort-subr nil
                 'end-of-defun
                 #'(lambda () nil)
                 #'(lambda () (forward-symbol 1) nil)
                 #'(lambda () (forward-symbol 1) nil)))))



(defun zv//initial-path-keybinding (key-file-map)
  "Create leader keybindings from an alist of the form (KEYS . PATH)"
  (mapc (lambda (binding)
          (let* ((path        (cdr binding))
                 (keybinding  (car binding)))
            ;; We check if it is an integer because keyseq returns a number if
            ;; the preceeding keys are also unbound.
            (evil-leader/set-key keybinding (if (string-match "\/$" path)
                                                ;; use ido-find-file-in-dir if we're binding a directory
                                                `(lambda () (interactive) (ido-find-file-in-dir ,path))
                                              ;; Otherwise we're looking at a file, jump directly to it
                                              `(lambda () (interactive) (find-file-existing ,path))))))
        key-file-map))


(defun zv/encrypt-secrets ()
  "Encrypt this file if it is in one of our `dirs-to-encrypt'"
  (require 'epa-mail)
  (let* ((zv-dotfiles (expand-file-name "~/Development/dotfilez/"))
         (files-to-encrypt (list (expand-file-name "~/.authinfo")))
         (dirs-to-encrypt (list (expand-file-name "~/.gnupg")
                                (expand-file-name (concat org-directory "/"))
                                (concat zv-dotfiles "gnupg/")
                                (concat zv-dotfiles "ssh/")
                                (expand-file-name "~/.ssh/")))
         (recipient (epg-list-keys (epg-make-context epa-protocol) "<zephyr.pellerin@gmail.com>" 'public)))
    (when (or (member (file-name-directory (buffer-file-name)) dirs-to-encrypt) (member buffer-file-name files-to-encrypt))
      (epa-encrypt-file (buffer-file-name) recipient))))


(defun zv/auto-publish ()
  "Automatically publish any ORG project files"
  (require 'ox-publish)
  (save-excursion
    (dolist (project org-publish-project-alist)
      (let ((dir (getf (cdr project) :base-directory))
            (file (buffer-file-name)))
        (if dir
            (if (file-in-directory-p file dir)
                (org-publish-file (buffer-file-name (buffer-base-buffer)))))))))

;; (remove-hook 'after-save-hook 'auto-publish)
(add-hook 'after-save-hook 'zv/auto-publish)

