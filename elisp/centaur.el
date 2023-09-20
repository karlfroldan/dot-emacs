(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-." . centaur-tabs-forward)
  ("C-c t n" . centaur-tabs--create-new-tab)
  ("C-c t k" . centaur-tabs--kill-this-buffer-dont-ask)
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
(setq centaur-tabs-style "wave")

;; Prevent access to some buffers
(defun centaur-tabs-hide-tab (x)
  "Do not show buffer X in tabs"
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window
     (window-dedicated-p (selected-window))
     ;; Buffer name not match below blacklist
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*Message" name)

     ;; Is not a magit buffer
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(setq centaur-tabsl-label-fixed-length 8)
