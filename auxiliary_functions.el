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



(defun my/fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if (match
         (let ((secret (plist-get match :secret)))
           (if (functionp secret)
               (funcall secret)
             secret))
         (error "Password not found for %S" params)))))

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

(cl-defmacro defsshserver (name
                             username
                             host
                             ; &optional (port "22")
                             &optional (directory "~")
                             &key (load-path '()))
  "Define an SSH server that can be called using ssh-name as specified by NAME.
The server will connect to USERNAME to HOST on the specified PORT.
The default DIRECTORY is the user's home."
  (let ((proto (if (eq system-type 'windows-nt)
                   "plink"
                 "ssh")))
    `(defun ,(intern (format "ssh-%s" name)) () ;(filename)
       (interactive)
             ;; (interactive "P\nFfile: ")
       ;; (let ((selected-file (if filename
       ;;                          filename
       ;;                        ,directory)))
         (dired (format "/%s:%s@%s:%s" ,proto ,username ,host ,directory)))))

(provide 'auxiliary_functions)
;;; auxiliary_functions.el ends here

