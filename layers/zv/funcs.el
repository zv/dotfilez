;; Empty the definition of some spacemacs init methods
(defun spacemacs/goto-link-line ())
(defun spacemacs//insert-banner ())



(defun zv/alter-window-by-dominant-dimension (magnitude)
  "Alter the current window by height if vertically split, or width otherwise"
  (cond ((window-full-width-p) (enlarge-window magnitude))
        ((window-full-height-p) (enlarge-window-horizontally magnitude))
        (t (enlarge-window (/ magnitude 2)))))

(defun zv/enlarge-window-by-dominant-dimension (&optional arg)
  "Enlarge the current window by height if vertically split, or width otherwise"
  (interactive "P")
  (zv/alter-window-by-dominant-dimension (if (numberp arg) arg 10)))

(defun zv/shrink-window-by-dominant-dimension (&optional arg)
  "Shrink the current window by height if vertically split, or width otherwise"
  (interactive "P")
  (zv/alter-window-by-dominant-dimension (if (numberp arg) (* -1 arg) -10)))

(defun zv/tile-split-window ()
  "If our current window width / height is greater than 1.68, split vertically"
  (interactive)
  (let* ((window-ratio (/ (float (window-pixel-width)) (float (window-pixel-height))))
         (golden-ratio (/ (+ 1 (sqrt 5)) 2)))
    (if (> window-ratio golden-ratio)
        (evil-window-vsplit)
      (evil-window-split))))



(defun zv/scroll-up-one-line ()
  (interactive)
  (scroll-up-line 1))

(defun zv/scroll-down-one-line ()
  (interactive)
  (scroll-down-line 1))



(defun zv/calculate-region (point mark)
  (interactive "r")
  (message (calc-eval (buffer-substring point mark))))

(defun zv/calculate-line ()
  (interactive)
  (message (calc-eval (buffer-substring (line-beginning-position) (line-end-position)))))


;; Org Mode
;; --------

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



(defun zv/search-parents-for-venv ()
  "Traverses upwards from buffer, looking to activate a virtualenv"
  (interactive)
  (let* ((base-dir (locate-dominating-file
                    buffer-file-name
                    (lambda (file) (file-in-directory-p "pyvenv.cfg" file))))
         (venv-dir (expand-file-name "venv" base-dir)))
    (if (and (stringp venv-dir)
             (file-exists-p (expand-file-name "venv/bin/activate" venv-dir)))
        (progn
          (pyvenv-activate venv-dir)
          (message "Activated %s as virtualenv" venv-dir))
      (user-error "Couldn't find a suitable venv"))))


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



(defun zv/bind-find-file (key path &rest bindings)
  "Create a binding which automatically visits PATH when KEY is pressed."
  (while key
    (let ((fname (intern (concat "path//" (file-relative-name path "~/")))))
      (defalias fname
        `(lambda ()
           (interactive)
           (let (path)
             (cond
              ((file-directory-p ,path) (ido-find-file-in-dir ,path))
              ((file-readable-p ,path) (find-file-existing ,path))
              (t (user-error "Couldn't open path %s" ,path))))))
      (spacemacs/set-leader-keys key fname))
    (setq key (pop bindings)
          path (pop bindings))))


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
