;;; auxiliary_functions --- Summary
;;; Commentary:
;;; Helper functions and macros I use for general Emacs stuff.
;;; Code:

(require 'seq)

(defun relative-emacs-dir (dir)
  "Return the absolute DIR from a path relative to emacs.d."
  (concat user-emacs-directory dir))

;; Add support for loading packages that do not play nice with Nix.
(defmacro use-package-with-load-path (pkg path &rest body)
  "Load PKG from the specified PATH."
  `(if (file-directory-p ,path)
     (use-package ,pkg
       :load-path ,path
       ,@body)
     (warn "use-package: load-path %s for package %s not found" ,path ,'pkg)))

(defun ensure-directory-exists (dir)
  "Create the directory DIR if it doesn't exist. Return an error if it does exists but not a directory"
  (cond ((file-directory-p dir)
         ;; Directory already exists - do nothing
         nil)
        ((file-exists-p dir)
         ;; Exists, but not a directory - error
         (error "Path exists but is not a directory: %s" dir))
        (t
         ;; Doesn't exist - create it
         (make-directory dir :parents))))

(defun package-recompile-all ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil))

(defun load-elisp-file (file-name)
  "Load an Emacs Lisp FILE-NAME that resides in the Emacs config directory."
  (if (file-exists-p file-name)
      (load (relative-emacs-dir file-name))
    (warn "File %s does not exist." file-name)))

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

(defun my/ssh--host-address (protocol username host &optional port)
  "Return a canonical host address string for SSH.
Format is "/PROTOCOL:USERNAME@HOST" or "/PROTOCOL:USERNAME@HOST#PORT" when PORT is provided."
  (if port
      (format "/%s:%s@%s#%d" protocol username host port)
    (format "/%s:%s@%s" protocol username host)))

(defun my/ssh--get-address (protocol username host port directory)
  "Build the full remote address including DIRECTORY for remote access.
Uses `my/ssh--host-address' and returns "HOST-ADDR:DIRECTORY"."
  (let ((host-addr (my/ssh--host-address protocol username host port)))
    (format "%s:%s" host-addr directory)))

(defun my/ssh--enter-dired (protocol username host port directory)
  "Open Dired on a remote DIRECTORY specified by PROTOCOL, USERNAME, HOST and PORT."
  (dired (my/ssh--get-address protocol username host port directory)))

(defun my/ssh--enter-shell (protocol username host port directory shell-program buffer-name)
  "Start a shell via SHELL-PROGRAM with default-directory set to the remote DIRECTORY.
The new shell buffer is renamed to BUFFER-NAME."
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
                           &key (load-path '())
                           &key (enable-projectile-project-root t))
  "Define an SSH server that can be called using ssh-name as specified by NAME.
The server will connect to USERNAME to HOST on the specified PORT.
The default DIRECTORY is the user's home."
  (let ((proto (my/ssh--ssh-protocol)))
    `(progn
       (defun ,(intern (format "ssh-%s" name)) (dir)
         "Start a dired ssh session"
         (interactive
          (list
           (read-directory-name "Directory: "
                                (my/ssh--get-address ,proto ,username ,host ,port ,default-directory))))
         (progn
           (let ((ssh-host (my/ssh--host-address ,proto ,username ,host ,port)))
             (if (and (not ,enable-projectile-project-root)
                      (not (member ssh-host my/projectile-project-root-remote-disable)))
                 (push ssh-host my/projectile-project-root-remote-disable)))
           (my/ssh--enter-dired ,proto ,username ,host ,port (file-remote-p dir 'localname))))

       (defun ,(intern (format "shell-%s" name)) (new-buffer-name)
         "Start a tramp shell session"
         (interactive (list (read-string "buffer name: " (concat "*shell-" ,name "*"))))
         (my/ssh--enter-shell ,proto
                              ,username
                              ,host
                              ,port
                              ,default-directory
                              ,shell-program new-buffer-name)))))

(defun my/shell-open-remote (username host)
  "Open an eat shell to HOST with user USERNAME."
  (interactive
   (list (read-string "username: ")
         (read-string "ssh host: ")))
  (let ((buffer-name (format "*shell:%s@%s*" username host))
        (protocol (my/ssh--ssh-protocol)))
    (my/ssh--enter-shell protocol username host 22 "~" "/bin/bash" buffer-name)))


;; TIME STUFF
(defun time-to-minutes (hour mins)
  (+ (* hour 60) mins))

(defun time-worked (begin end)
  (let ((time-begin (apply #'time-to-minutes (parse-time begin)))
        (time-end   (apply #'time-to-minutes (parse-time end))))
    (- time-end time-begin)))

(defun mins-to-hours (mins)
  (list (/ mins 60) (mod mins 60)))

(defun parse-time (time)
  (mapcar #'string-to-number (split-string time ":")))

(provide 'auxiliary_functions)
;;; auxiliary_functions.el ends here

