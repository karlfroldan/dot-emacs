;;; auxiliary_functions --- Summary
;;; Commentary:
;;; Helper functions and macros I use for general Emacs stuff.
;;; Code:

(require 'seq)

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

(defun my/ssh--get-address (protocol username host port directory)
  "Get the canonical address used to connect to a remote host via SSH"
  (if port
      (format "/%s:%s@%s#%d:%s" protocol username host port directory)
    (format "/%s:%s@%s:%s" protocol username host directory)))

(defun my/ssh--enter-dired (protocol username host port directory)
  (dired (my/ssh--get-address protocol username host port directory)))

(defun my/ssh--enter-shell (protocol username host port directory shell-program buffer-name)
  (let* ((default-directory (my/ssh--get-address protocol username host port directory))
         (new-eat-buffer (eat shell-program)))
    (with-current-buffer new-eat-buffer
      (rename-buffer buffer-name t))))

(defun my/ssh--ssh-protocol ()
  "Determine the SSH protocol to use to connect to the remote server."
  (if (eq system-type 'windows-nt)
      "plink"
    "ssh"))

(cl-defmacro defsshserver (name
                           username
                           host
                           &key (port nil)
                           &key (default-directory "~/")
                           &key (shell-program "/bin/sh")
                           &key (load-path '()))
  "Define an SSH server that can be called using ssh-name as specified by NAME.
The server will connect to USERNAME to HOST on the specified PORT.
The default DIRECTORY is the user's home."
  (let ((proto (my/ssh--ssh-protocol)))
    `(progn
       (defun ,(intern (format "ssh-%s" name)) (dir)
         "Start a dired ssh session"
         (interactive
          (list (read-directory-name "Directory: " (my/ssh--get-address ,proto ,username ,host ,port ,default-directory))))
         (my/ssh--enter-dired ,proto ,username ,host ,port (file-remote-p dir 'localname)))

       (defun ,(intern (format "shell-%s" name)) (new-buffer-name)
         "Start a tramp shell session"
         (interactive (list (read-string "buffer name: " (concat "*shell-" ,name "*"))))
         (my/ssh--enter-shell ,proto ,username ,host ,port ,default-directory ,shell-program new-buffer-name)))))

(defun my/shell-open-remote (username host)
  "Open an eat shell to HOST with user USERNAME."
  (interactive
   (list (read-string "username: ")
         (read-string "ssh host: ")))
  (let ((buffer-name (format "*shell:%s@%s*" username host))
        (protocol (my/ssh--ssh-protocol)))
    (my/ssh--enter-shell protocol username host 22 "~" "/bin/bash" buffer-name)))

(provide 'auxiliary_functions)
;;; auxiliary_functions.el ends here

