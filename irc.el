;; (load "~/.emacs.d/.ercpass.gpg")

(require 'erc-services)
(erc-services-mode 1)
(erc-log-mode)

(defun my/irc-buffer-name (server port)
  (concat server ":" port))

(defun my/buffer-name-from-server (server)
  "Simple string conversion.

A function to convert SERVER such as
\=irc.libera.chat\= to \=Libera.Chat\=."
  (let* ((parts (split-string server "\\."))
         (capitalized (mapcar #'capitalize (cdr parts))))
    (string-join capitalized ".")))

(cl-defmacro defirc (name server
                          &optional
                          (port "6697"))
  `(defun ,(intern (format "irc-%s" name)) ()
     (interactive)
     (erc-tls :server ,server
              :port ,port)
     (switch-to-buffer (my/irc-buffer-name ,server ,port))))
;;     (switch-to-buffer (my/buffer-name-from-server ,server))))

(setq
 ;;; BASIC IRC SETUP
 erc-nick "fireking04"
 
 erc-prompt-for-nickserv-password nil
 
 ;;; IRC LOGGING
 ;; IRC log file format is #channel@server.txt
 erc-generate-log-file-name-function 'erc-generate-log-file-name-network
 ;; Always write to the log file after sending the message
 erc-log-write-after-send t
 ;; Write logs after quitting the channel
 erc-save-buffer-on-part t
 
 erc-log-channels-directory "~/.erc/logs/"

 ;; When doing /part or /quit in a channel, kill the buffer
 erc-kill-buffer-on-part t
 erc-kill-queries-on-part t
 erc-kill-server-buffer-on-quit t

 ;;; IRC NOTIFICATIONS
 ;; Enable DBus notifications
 erc-notifications-mode t)

(defirc "libera-chat" "irc.libera.chat")

(setq erc-autojoin-channels-alist
      '(("libera.chat" "#emacs" "#gentoo" "#gentoo-chat")))

(provide 'irc)
