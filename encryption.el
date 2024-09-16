(use-package epg
  :custom ((epg-pinentry-mode 'loopback)))

(use-package epa
  :after epg
  :custom ((epa-file-encrypt-to user-mail-address))
  :config
  (setq-default epa-file-select-keys (list user-mail-address))
  (setenv "GPA_AGENT_INFO" nil))

