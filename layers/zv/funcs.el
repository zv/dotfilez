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


(defun zv/calculate-expression (beginning end)
  "Return the result of evaluating the current expression, if there is one"
  (interactive "r")
  (message "Result: %s"
           (kill-new
            (calc-eval
             ;; evaluate the expression inside the region selected.
             (if (use-region-p)
                 (buffer-substring-no-properties beginning end)
               ;; if nothing's selected, use the line
               (thing-at-point 'line t))))))



(defun zv/search-parents-for-venv ()
  "Traverses upwards from buffer, looking to activate a virtualenv"
  (interactive)
  (when-let* ((base-dir (locate-dominating-file
                       buffer-file-name
                       (lambda (dir) (file-expand-wildcards (concat dir "/*/bin/activate") t))))
            (venv-dir (expand-file-name "venv" base-dir))
            (version (s-match "version =.*" (f-read (expand-file-name "pyvenv.cfg" venv-dir)))))
        (pyvenv-activate venv-dir)
        (setq importmagic-python-interpreter (concat venv-dir "/bin/python"))
        (message "Found venv [`%s'] at %s packaged with python %s." pyvenv-virtual-env-name venv-dir version)))



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
