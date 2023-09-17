(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward)
  ("C-c t n" . centaur-tabs--create-new-tab))

;; Tab height
(setq centaur-tabs-height 32)
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-plain-icons t)
(setq centaur-tabs-style "bar")
;;(setq centaur-tabs-enable-key-bindings t)
