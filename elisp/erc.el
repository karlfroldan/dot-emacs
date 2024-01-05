(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)

(setq
    erc-nick "fireking04"
    erc-user-full-name "fireking04")

(defun libera-chat-serv ()
    (interactive)
    (erc-tls :server "irc.libera.chat"
        :port "6697"))


