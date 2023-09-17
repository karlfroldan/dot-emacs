(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward)
  ("C-c t n" . centaur-tabs--create-new-tab)
  :hook
  ;; Disable centaur in these modes
  (dired-mode . centaur-tabs-local-mode)
  (message-mode . centaur-tabs-local-mode)
  (help-mode . centaur-tabs-local-mode)
  (debugger-mode . centaur-tabs-local-mode))

;; Tab height
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-style "bar")
;;(setq centaur-tabs-enable-key-bindings t)
