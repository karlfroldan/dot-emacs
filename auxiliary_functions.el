;;; Helper functions and macros I use for general emacs stuff.

(defun relative-emacs-dir (dir)
  "Return the absolute directory from a path relative to emacs.d"
  (concat (getenv "HOME") "/.emacs.d/" dir))

(defun package-recompile-all ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil))

(defun load-elisp-file (file-name)
  "Load an Emacs LISP file that resides in the emacs config directory"
  (load (relative-emacs-dir file-name)))

(defun bootstrap-emacs ()
  (interactive)
  (mapc #'package-install package-selected-packages)
  (all-the-icons-install-fonts)
  (write-region "" nil (relative-emacs-dir ".bootstrapped")))

(cl-defmacro add-ssh-server (name
                             username
                             host
                             &optional (port "22")
                             &optional (directory "~"))
  `(defun ,(intern (format "ssh-%s" name)) ()
     (interactive)
     (dired ,(format "/ssh:%s@%s#%s:%s" username host port directory))))
