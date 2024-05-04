;; (load "~/.emacs.d/.ercpass.gpg")

;; (require 'erc-services)
;; (erc-services-mode 1)

(cl-defmacro defirc (name server &optional (port "6697"))
  `(defun ,(intern (format "irc-%s" name)) ()
     (interactive)
     (erc-tls :server ,server
              :port ,port)))

(setq
 ;;; BASIC IRC SETUP
 ;; IRC Nickname
 erc-nick "fireking04"
 ;; /whois name
 erc-user-full-name "fireking04"
 
 ;;; IRC LOGGING
 ;; Enable IRC logging
 erc-log-mode t
 ;; IRC log file format is #channel@server.txt
 erc-generate-log-file-name-function 'erc-generate-log-file-name-network
 ;; Always write to the log file after sending the message

 ;;; IRC NOTIFICATIONS
 erc-log-write-after-send t
 ;; Enable DBus notifications
 erc-notifications-mode t)

(defirc "libera-chat" "irc.libera.chat")
