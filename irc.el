(defun my/circe--fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun nickserv-password (server)
  (my/circe--fetch-password :user "fireking04" :machine server))

(use-package circe
  :custom
  ((circe-network-options '(("Libera Chat"
                             :use-tls t
                             :channels ("#emacs")
                             :realname "fireking04"
                             :nick "fireking04"
                             :nickserv-password nickserv-password)))))
