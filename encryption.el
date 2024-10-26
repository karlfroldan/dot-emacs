(setq epg-pinentry-mode 'loopback)

(use-package epa
  :after epg
  :custom ((epa-file-encrypt-to user-mail-address))
  :config
  (setq-default epa-file-select-keys (list user-mail-address))
  (setenv "GPG_AGENT_INFO" nil))

(require 'epa-file)
(epa-file-enable)


