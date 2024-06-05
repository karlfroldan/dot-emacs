;;; auxiliary_functions --- Summary
;;; Commentary:
;;; Helper functions and macros I use for general Emacs stuff.
;;; Code:

(defun relative-emacs-dir (dir)
  "Return the absolute DIR from a path relative to emacs.d."
  (concat user-emacs-directory dir))

(defun package-recompile-all ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil))

(defun load-elisp-file (file-name)
  "Load an Emacs Lisp FILE-NAME that resides in the Emacs config directory."
  (load (relative-emacs-dir file-name)))

(cl-defmacro defsshserver (name
                             username
                             host
                             &optional (port "22")
                             &optional (directory "~"))
  "Define an SSH server that can be called using ssh-name as specified by NAME.
The server will connect to USERNAME to HOST on the specified PORT.
The default DIRECTORY is the user's home."
  (let ((proto (if (eq system-type 'windows-nt)
                   "plink"
                 "ssh")))
    `(defun ,(intern (format "ssh-%s" name)) ()
       (interactive)
       (dired ,(format "/%s:%s@%s#%s:%s" proto username host port directory)))))

(defmacro my/load-make-after-frame (&rest fns)
  "Load set of functions FNS during after-make-frame."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (when (display-graphic-p)
                       (progn ,@fns)))))
     (when (display-graphic-p)
       (progn ,@fns))))

(provide 'auxiliary_functions)
;;; auxiliary_functions.el ends here

